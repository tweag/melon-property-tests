{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Melon.Contract.PriceFeed
  ( deploy
  , deployCanonicalPriceFeed
  , deployStakingPriceFeed
  , updateCanonicalPriceFeed
  , makeConstantConvertedPrices
  ) where

import Control.Exception.Safe (Exception (..), throw)
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Lens
import qualified Data.HashMap.Strict as HashMap
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Traversable (for)
import Data.Typeable (Typeable)
import Network.Ethereum.ABI.Prim.Address (Address)
import Network.Ethereum.ABI.Prim.Int (UIntN)
import Network.Ethereum.Web3.Types (Call (..))
import Network.Wreq
import Numeric.Decimal
import Text.Read (readMaybe)

import Melon.Context
import qualified Melon.ABI.Assets.Asset as Asset
import qualified Melon.ABI.PriceFeeds.CanonicalPriceFeed as CanonicalPriceFeed
import qualified Melon.ABI.PriceFeeds.StakingPriceFeed as StakingPriceFeed
import qualified Melon.ABI.System.OperatorStaking as OperatorStaking
import Melon.ThirdParty.Network.Ethereum.Web3.Eth


deploy
  :: MonadMelon m
  => Address
    -- ^ Owner
  -> Address
    -- ^ Melon token address
  -> Address
    -- ^ Governance contract address
  -> m (Address, Address)
    -- ^ Returns canonical and staking price feed addresses
deploy owner mlnToken governance = do
  canonicalPriceFeed <- deployCanonicalPriceFeed owner mlnToken governance
  stakingPriceFeed <- deployStakingPriceFeed owner mlnToken canonicalPriceFeed

  defaultCall <- getCall
  let ownerCall = defaultCall { callFrom = Just owner }
      amountToStake = 1000000 :: UIntN 256

  -- Approve staking price feed spending
  let ownerCallMln = ownerCall { callTo = Just mlnToken }
  liftWeb3 $
    Asset.approve ownerCallMln stakingPriceFeed amountToStake
    >>= getTransactionEvents >>= \case
      [Asset.Approval {}] -> pure ()
      _ -> throw FailedToApproveStakingPriceFeed

  -- Deposit stake on staking price feed
  let ownerCallStaking = ownerCall { callTo = Just stakingPriceFeed }
  liftWeb3 $
    StakingPriceFeed.depositStake ownerCallStaking amountToStake ""
    >>= getTransactionEvents >>= \case
      [OperatorStaking.Staked {}] -> pure ()
      _ -> throw FailedToDepositStake

  pure (canonicalPriceFeed, stakingPriceFeed)

deployCanonicalPriceFeed
  :: MonadMelon m
  => Address
    -- ^ Owner
  -> Address
    -- ^ Melon token address
  -> Address
    -- ^ Governance contract address
  -> m Address
    -- ^ Returns price feed address
deployCanonicalPriceFeed owner mlnToken governance = do
  defaultCall <- getCall
  let ownerCall = defaultCall { callFrom = Just owner }
  tx <- liftWeb3 $ CanonicalPriceFeed.constructor ownerCall
    mlnToken -- quote asset
    "Melon Token" -- quote asset name
    "MLN-T" -- quote asset symbol
    18 -- quote asset decimal places
    "melonport.com" -- quote asset related URL
    mockBytes -- quote asset IPFS hash
    mockAddress -- quote asset break-in address
    mockAddress -- quote asset break-out address
    [] -- quote asset EIP standards
    [] -- quote asset white listed functions
    [0, 60] -- update-info: interval, validity
    [1000000, 4] -- staking-info: minStake, numOperators
    governance -- address of Governance
  liftWeb3 $ getContractAddress tx

deployStakingPriceFeed
  :: MonadMelon m
  => Address
    -- ^ Owner
  -> Address
    -- ^ Melon token address
  -> Address
    -- ^ CanonicalPriceFeed address
  -> m Address
    -- ^ Returns price feed address
deployStakingPriceFeed owner mlnToken canonicalPriceFeed = do
  defaultCall <- getCall
  let ownerCall = defaultCall { callFrom = Just owner }
  tx <- liftWeb3 $ StakingPriceFeed.constructor ownerCall
    canonicalPriceFeed -- canonical registrar address
    mlnToken -- quote asset
    canonicalPriceFeed -- superfeed address
  liftWeb3 $ getContractAddress tx


mockBytes :: T.Text
mockBytes = "0x86b5eed81db5f691c36cc83eb58cb5205bd2090bf3763a19f0c5bf2f074dd84b"

mockAddress :: Address
mockAddress = "0x083c41ea13af6c2d5aaddf6e73142eb9a7b00183"

-- | A price source
--
-- Takes @quoteName quoteDecimals tokens@(name, decimals)@
-- and returns the list of prices.
type PriceSource = IO [UIntN 256]

-- | Fetch current prices from the given source and update the price-feed.
updateCanonicalPriceFeed
  :: MonadMelon m
  => PriceSource
  -> Address -- ^ CanonicalPriceFeed contract address
  -> Address -- ^ StakingPriceFeed contract address
  -> Address -- ^ Contract owner address
  -> [Address] -- ^ List of token addresses
  -> m ()
updateCanonicalPriceFeed source canonical staking owner assets = do

  prices <- liftIO source

  defaultCall <- getCall
  let ownerCallStaking = defaultCall
        { callFrom = Just owner
        , callTo = Just staking
        }
  liftWeb3 $ StakingPriceFeed.update ownerCallStaking
    assets -- list of asset addresses
    prices -- list of asset prices
    >>= getTransactionEvents >>= \case
      [StakingPriceFeed.PriceUpdated _] -> pure ()
      _ -> throw ExpectedOnePriceUpdateEvent

  let ownerCallCanonical = defaultCall
        { callFrom = Just owner
        , callTo = Just canonical
        }
  liftWeb3 $ CanonicalPriceFeed.collectAndUpdate ownerCallCanonical
    assets -- list of asset addresses
    >>= getTransactionEvents >>= \case
      [CanonicalPriceFeed.PriceUpdated _] -> pure ()
      _ -> throw ExpectedOnePriceUpdateEvent
  pure ()


type BigNumber = Decimal P18 RoundHalfUp

roundTo15 :: BigNumber -> BigNumber
roundTo15 = cast . (cast :: Decimal P18 RoundHalfUp -> Decimal P15 RoundHalfUp)


-- | Fetches fresh prices from crypto compare.
getConvertedPrices
  :: T.Text -- ^ Quote asset name on cryptocompare
  -> Integer -- ^ Quote asset decimals
  -> [(T.Text, Integer)] -- ^ List of tokens (name on cryptocompare, decimals)
  -> PriceSource -- ^ Returns list of prices (in tokens order)
getConvertedPrices quoteName quoteDecimals tokens = do
  priceMap <- do
    let names = map fst tokens
        tsyms = T.intercalate "," names
        url = "https://min-api.cryptocompare.com/data/price?fsym="
          <> quoteName <> "&tsyms=" <> tsyms <> "&sign=true"
    r <- get (T.unpack url)
    pure $ HashMap.fromList $
      r^@..responseBody.members._Number.to show.to readMaybe._Just
  let mbPrices = for tokens $ \(name, decimals) ->
        case HashMap.lookup name priceMap of
          Nothing -> Nothing
          Just (price :: BigNumber) -> Just $
            let inverse = roundTo15 (1 / price)
                divided = inverse / (10^^(decimals - quoteDecimals))
            in truncate $ divided * (10^^decimals)
  maybe (throw CryptoCompareMissingCurrency) return mbPrices


-- | Fetches fresh prices from crypto compare
-- and returns a function that will always return these prices.
makeConstantConvertedPrices
  :: T.Text -- ^ Quote asset name on cryptocompare
  -> Integer -- ^ Quote asset decimals
  -> [(T.Text, Integer)] -- ^ List of tokens (name on cryptocompare, decimals)
  -> IO PriceSource
makeConstantConvertedPrices quoteName quoteDecimals tokens = do
  prices <- getConvertedPrices quoteName quoteDecimals tokens
  pure $ pure prices


-- | Dummy price-feed update that just returns constants.
--
-- The values returned were taken from a run of @utils/lib/updatePriceFeed.js@.
-- This function assumes
--
-- Example output from @utils/lib/updatePriceFeed.js@
-- MLN price 1000000000000000000
-- ETH price 13802622498274673000
-- EUR price 28224668360147000
--
-- Example output from 'getConvertedPrices' above.
-- MLN price 1000000000000000000
-- ETH price 13844662882458800000
-- EUR price 28304557033682400
-- getConvertedPrices
--   :: T.Text
--   -> Integer
--   -> [(T.Text, Integer)]
--   -> IO [UIntN 256]
-- getConvertedPrices "MLN" 18 [("MLN", _), ("ETH", _), ("EUR", _)] =
--   pure
--     [ 1000000000000000000  -- MLN
--     , 13802622498274673000  -- ETH
--     , 28224668360147000  -- EUR
--     ]
-- getConvertedPrices _ _ _ = error "[getConvertedPrices (dummy)] Invalid input."


data PriceFeedError
  = CryptoCompareInvalidResult String
  | CryptoCompareMissingCurrency
  | ConstantMissingCurrency
  | ExpectedOnePriceUpdateEvent
  | FailedToApproveStakingPriceFeed
  | FailedToDepositStake
  deriving (Show, Typeable)
instance Exception PriceFeedError where
  displayException = \case
    CryptoCompareInvalidResult msg ->
      "Price lookup on cryptocompare returned an invalid result: " ++ msg
    CryptoCompareMissingCurrency ->
      "Price lookup on cryptocompare was missing a currency."
    ConstantMissingCurrency ->
      "Price lookup in constant price-map was missing a currency."
    ExpectedOnePriceUpdateEvent ->
      "Expected exactly one `PriceUpdate` event."
    FailedToApproveStakingPriceFeed ->
      "Failed to approve staking price feed."
    FailedToDepositStake ->
      "Failed to deposity stake on staking price feed."

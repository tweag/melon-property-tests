{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Melon.Contract.PriceFeed
  ( deploy
  , deployCanonicalPriceFeed
  , deployStakingPriceFeed
  , updateCanonicalPriceFeed
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
import Network.Ethereum.Web3.Provider (Web3)
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
  :: Address
    -- ^ Owner
  -> Address
    -- ^ Melon token address
  -> Address
    -- ^ Governance contract address
  -> MelonT Web3 (Address, Address)
    -- ^ Returns canonical and staking price feed addresses
deploy owner mlnToken governance = do
  canonicalPriceFeed <- deployCanonicalPriceFeed owner mlnToken governance
  stakingPriceFeed <- deployStakingPriceFeed owner mlnToken canonicalPriceFeed

  defaultCall <- view ctxCall
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
  :: Address
    -- ^ Owner
  -> Address
    -- ^ Melon token address
  -> Address
    -- ^ Governance contract address
  -> MelonT Web3 Address
    -- ^ Returns price feed address
deployCanonicalPriceFeed owner mlnToken governance = withContext $ \ctx -> do
  let ownerCall = (ctx^.ctxCall) { callFrom = Just owner }
  tx <- CanonicalPriceFeed.constructor ownerCall
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
  getContractAddress tx

deployStakingPriceFeed
  :: Address
    -- ^ Owner
  -> Address
    -- ^ Melon token address
  -> Address
    -- ^ CanonicalPriceFeed address
  -> MelonT Web3 Address
    -- ^ Returns price feed address
deployStakingPriceFeed owner mlnToken canonicalPriceFeed = withContext $ \ctx -> do
  let ownerCall = (ctx^.ctxCall) { callFrom = Just owner }
  tx <- StakingPriceFeed.constructor ownerCall
    canonicalPriceFeed -- canonical registrar address
    mlnToken -- quote asset
    canonicalPriceFeed -- superfeed address
  getContractAddress tx


mockBytes :: T.Text
mockBytes = "0x86b5eed81db5f691c36cc83eb58cb5205bd2090bf3763a19f0c5bf2f074dd84b"

mockAddress :: Address
mockAddress = "0x083c41ea13af6c2d5aaddf6e73142eb9a7b00183"



-- | Fetch current prices from cryptocompare and update the price-feed.
updateCanonicalPriceFeed
  :: Address -- ^ CanonicalPriceFeed contract address
  -> Address -- ^ StakingPriceFeed contract address
  -> Address -- ^ Contract owner address
  -> T.Text -- ^ Quote asset name on cryptocompare
  -> Integer -- ^ Quote asset decimals
  -> [(T.Text, Address, Integer)] -- ^ List of tokens (name on cryptocompare, address, decimals)
  -> MelonT Web3 ()
updateCanonicalPriceFeed canonical staking owner quoteName quoteDecimals tokens =
  withContext $ \ctx -> do

  let namesDecimals = [ (name, decimals) | (name, _, decimals) <- tokens ]
      assets = [ addr | (_, addr, _) <- tokens ]
  prices <- liftIO $ getConvertedPrices quoteName quoteDecimals namesDecimals

  let ownerCallStaking = (ctx^.ctxCall)
        { callFrom = Just owner
        , callTo = Just staking
        }
  StakingPriceFeed.update ownerCallStaking
    assets -- list of asset addresses
    prices -- list of asset prices
    >>= getTransactionEvents >>= \case
      [StakingPriceFeed.PriceUpdated _] -> pure ()
      _ -> throw ExpectedOnePriceUpdateEvent

  let ownerCallCanonical = (ctx^.ctxCall)
        { callFrom = Just owner
        , callTo = Just canonical
        }
  CanonicalPriceFeed.collectAndUpdate ownerCallCanonical
    assets -- list of asset addresses
    >>= getTransactionEvents >>= \case
      [CanonicalPriceFeed.PriceUpdated _] -> pure ()
      _ -> throw ExpectedOnePriceUpdateEvent
  pure ()


type BigNumber = Decimal P18 RoundHalfUp

roundTo15 :: BigNumber -> BigNumber
roundTo15 = cast . (cast :: Decimal P18 RoundHalfUp -> Decimal P15 RoundHalfUp)


getConvertedPrices
  :: T.Text -- ^ Quote asset name on cryptocompare
  -> Integer -- ^ Quote asset decimals
  -> [(T.Text, Integer)] -- ^ List of tokens (name on cryptocompare, decimals)
  -> IO [UIntN 256]
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
    ExpectedOnePriceUpdateEvent ->
      "Expected exactly one `PriceUpdate` event."
    FailedToApproveStakingPriceFeed ->
      "Failed to approve staking price feed."
    FailedToDepositStake ->
      "Failed to deposity stake on staking price feed."

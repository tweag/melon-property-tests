{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Melon.Contract.PriceFeed
  ( deploy
  , deployCanonicalPriceFeed
  , deployStakingPriceFeed
  , updatePriceFeed
  , PriceFeedSpec (..)
  , createPriceFeed
  ) where

import Control.Exception.Safe (Exception (..), throw)
import Control.Lens
import Data.Aeson.Lens
import Data.HashMap.Strict (HashMap)
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
import Melon.Model.Input
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
    -- update-info: interval, validity
    -- Real-time requirements such as "update every 60 s" are difficult to
    -- combine with the property based state-machine testing used in this
    -- project.  Currently, these requirements are ignored by configuring a
    -- zero interval and very long validity time. The interval defines how much
    -- time needs to have passed between investment request and execution, and
    -- the validity time defines how long a price-feed update is considered
    -- valid. The tests do perform price-feed updates. So, that aspect is not
    -- untested.
    [0, oneYearInSeconds]
    [1000000, 4] -- staking-info: minStake, numOperators
    governance -- address of Governance
  liftWeb3 $ getContractAddress tx
  where
    oneYearInSeconds = 60 * 60 * 24 * 365

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

-- | Fetch current prices from the given source and update the price-feed.
updatePriceFeed
  :: MonadMelon m
  => VersionDeployment -- ^ Version contract deployment
  -> (HashMap Address (UIntN 256)) -- ^ The new asset prices
  -> m ()
updatePriceFeed version prices = do
  defaultCall <- getCall
  let canonical = version^.vdCanonicalPriceFeed
      staking = version^.vdStakingPriceFeed
      owner = version^.vdOwner

  -- Update staking price feed
  let ownerCallStaking = defaultCall
        { callFrom = Just owner
        , callTo = Just staking
        }
  liftWeb3 $ StakingPriceFeed.update ownerCallStaking
    (HashMap.keys prices) -- list of asset addresses
    (HashMap.elems prices) -- list of asset prices
    >>= getTransactionEvents >>= \case
      [StakingPriceFeed.PriceUpdated _] -> pure ()
      _ -> throw $ ExpectedOnePriceUpdateEvent
        "At StakingPriceFeed.update"

  -- Update canonical price feed
  let ownerCallCanonical = defaultCall
        { callFrom = Just owner
        , callTo = Just canonical
        }
  liftWeb3 $ CanonicalPriceFeed.collectAndUpdate ownerCallCanonical
    (HashMap.keys prices) -- list of asset addresses
    >>= getTransactionEvents >>= \case
      [CanonicalPriceFeed.PriceUpdated _] -> pure ()
      _ -> throw $ ExpectedOnePriceUpdateEvent
        "At CanonicalPriceFeed.collectAndUpdate"


type BigNumber = Decimal P18 RoundHalfUp

roundTo15 :: BigNumber -> BigNumber
roundTo15 = cast . (cast :: Decimal P18 RoundHalfUp -> Decimal P15 RoundHalfUp)


-- | Fetches fresh prices from crypto compare.
getConvertedPrices
  :: T.Text -- ^ Quote asset name on cryptocompare
  -> Integer -- ^ Quote asset decimals
  -> (HashMap Address AssetSpec) -- ^ Known tokens
  -> IO (HashMap Address (UIntN 256)) -- ^ Returns asset prices
getConvertedPrices quoteName quoteDecimals tokens = do
  (priceMap :: HashMap T.Text BigNumber) <- do
    let names :: [T.Text]
        names = tokens^..traverse.asCryptoCompareName
        tsyms = T.intercalate "," names
        url = "https://min-api.cryptocompare.com/data/price?fsym="
          <> quoteName <> "&tsyms=" <> tsyms <> "&sign=true"
    r <- get (T.unpack url)
    pure $ HashMap.fromList $
      r^@..responseBody.members._Number.to show.to readMaybe._Just
  let mbPrices = for tokens $ \spec ->
        case priceMap^.at (spec^.asCryptoCompareName) of
          Nothing -> Nothing
          Just (price :: BigNumber) -> Just $
            let decimals :: Integer
                decimals = spec^.asDecimals
                inverse :: BigNumber
                inverse = roundTo15 (1 / price)
                divided :: BigNumber
                divided = inverse / (10^^(decimals - quoteDecimals))
            in truncate $ divided * (10^^decimals) :: UIntN 256
  maybe (throw CryptoCompareMissingCurrency) return mbPrices


data PriceFeedSpec
    -- | Fetches prices from CryptoCompare once, then repeats those prices.
  = RealisticConstantPriceFeed
      T.Text -- ^ Quote asset name on CryptoCompare
      Integer -- ^ Quote asset decimals
      (HashMap Address AssetSpec) -- ^ Known assets
    -- | Ever repeating cycle of unrealistic prices
  | UnrealisticCyclicPriceFeed [HashMap Address (UIntN 256)]
  deriving (Eq, Ord, Show)


createPriceFeed :: PriceFeedSpec -> IO [HashMap Address (UIntN 256)]
createPriceFeed = \case
  RealisticConstantPriceFeed quoteName quoteDecimals tokens -> do
    prices <- getConvertedPrices quoteName quoteDecimals tokens
    pure $ cycle [prices]
  UnrealisticCyclicPriceFeed priceCycle ->
    pure $ cycle priceCycle


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
  | ExpectedOnePriceUpdateEvent String
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
    ExpectedOnePriceUpdateEvent msg ->
      "Expected exactly one `PriceUpdate` event: "
      ++ msg
    FailedToApproveStakingPriceFeed ->
      "Failed to approve staking price feed."
    FailedToDepositStake ->
      "Failed to deposity stake on staking price feed."

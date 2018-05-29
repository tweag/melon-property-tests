{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Melon.Contract.PriceFeed
  ( updateCanonicalPriceFeed
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
import qualified Melon.ABI.PriceFeeds.CanonicalPriceFeed as CanonicalPriceFeed
import qualified Melon.ABI.PriceFeeds.StakingPriceFeed as StakingPriceFeed
import Melon.ThirdParty.Network.Ethereum.Web3.Eth


data PriceFeedError
  = CryptoCompareInvalidResult String
  | CryptoCompareMissingCurrency
  | ExpectedOnePriceUpdateEvent
  deriving (Show, Typeable)
instance Exception PriceFeedError where
  displayException = \case
    CryptoCompareInvalidResult msg ->
      "Price lookup on cryptocompare returned an invalid result: " ++ msg
    CryptoCompareMissingCurrency ->
      "Price lookup on cryptocompare was missing a currency."
    ExpectedOnePriceUpdateEvent ->
      "Expected exactly one `PriceUpdate` event."


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

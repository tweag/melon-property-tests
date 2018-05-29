{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Melon.Contract.PriceFeed
  ( updateCanonicalPriceFeed
  ) where

import Control.Exception.Safe (Exception (..), throw)
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import qualified CryptoCompare
import qualified Data.Map.Lazy as Map
import Data.Typeable (Typeable)
import Network.Ethereum.ABI.Prim.Address (Address)
import Network.Ethereum.ABI.Prim.Int (UIntN)
import Network.Ethereum.Web3.Types (Call (..))

import Melon.Context
import qualified Melon.ABI.PriceFeeds.CanonicalPriceFeed as CanonicalPriceFeed
import qualified Melon.ABI.PriceFeeds.StakingPriceFeed as StakingPriceFeed
import Melon.ThirdParty.Network.Ethereum.Web3.Eth


data PriceFeedError
  = CryptoCompareLookupFailed String
  | CryptoCompareMissingCurrency
  | ExpectedOnePriceUpdateEvent
  deriving (Show, Typeable)
instance Exception PriceFeedError where
  displayException = \case
    CryptoCompareLookupFailed msg ->
      "Price lookup on cryptocompare failed: " ++ msg
    CryptoCompareMissingCurrency ->
      "Price lookup on cryptocompare was missing a currency."
    ExpectedOnePriceUpdateEvent ->
      "Expected exactly one `PriceUpdate` event."


-- | Fetch current prices from cryptocompare and update the price-feed.
updateCanonicalPriceFeed
  :: Address -- ^ CanonicalPriceFeed contract address
  -> Address -- ^ StakingPriceFeed contract address
  -> Address -- ^ Contract owner address
  -> String -- ^ Quote asset name on cryptocompare
  -> [(String, Address, UIntN 256)] -- ^ List of tokens (name on cryptocompare, address, decimals)
  -> MelonM ()
updateCanonicalPriceFeed canonical staking owner quote tokens =
  withContext $ \ctx -> do

  let names = [ name | (name, _, _) <- tokens ]
  resp <- liftIO $ CryptoCompare.fetchCurrentPrice quote names
  prices <- case resp of
    Left err -> throw $ CryptoCompareLookupFailed err
    Right (CryptoCompare.PriceResponse pricemap) ->
      case traverse (`Map.lookup` pricemap) names of
        Nothing -> throw CryptoCompareMissingCurrency
        Just prices -> pure $ zipWith
          -- XXX: CryptoCompare returns 'Float' which introduces some rounding errors.
          (\(_, _, dec) price -> round $ price * fromIntegral (10^dec::Integer))
          tokens prices
  let assets = [ addr | (_, addr, _) <- tokens ]

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

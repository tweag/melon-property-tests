{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Melon.Test.Commands where

import Control.Exception.Safe (MonadCatch, MonadThrow)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Data.Proxy (Proxy (..))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.Ethereum.ABI.Prim.Address (Address)
import Network.Ethereum.ABI.Prim.Int (UIntN)
import Network.Ethereum.Web3.Provider (Web3)
import Network.Ethereum.Web3.Types (Call (..))

import qualified Melon.ABI.Assets.Asset as Asset
import qualified Melon.ABI.Exchange.Adapter.MatchingMarketAdapter as MatchingMarketAdapter
import qualified Melon.ABI.Fund as Fund
import qualified Melon.ABI.PriceFeeds.CanonicalPriceFeed as CanonicalPriceFeed
import Melon.ThirdParty.Network.Ethereum.ABI.Codec (encodeSignature)
import Melon.ThirdParty.Network.Ethereum.Web3.Eth (getTransactionEvents)
import Melon.Context
import Melon.Deploy (deploy)
import Melon.Model


tests :: IO Bool
tests = checkParallel $$discover


prop_melonport :: Property
prop_melonport = withTests 10 $ property $ do
  -- XXX: This repeats the full deployment for each test run.
  --   It might be better (certainly for performance) to only setup a new fund
  --   for each test-case, and share the same 'Version' instance between tests.
  -- XXX: Use a mocked external price feed. Calling out the cryptocompare
  --   every time is too slow. Could be randomly generated, which might be good
  --   for test-coverage.
  (version, fund, priceFeed, updatePriceFeed, manager, assets) <- liftIO deploy
  let mlnToken = version^.vdMlnToken
      ethToken = version^.vdEthToken
  actions <- forAll $
    Gen.sequential (Range.linear 1 100) initialModelState
      [ checkSharePrice fund priceFeed assets
      , checkFeeAllocation fund
      , checkMakeOrderSharePrice fund manager updatePriceFeed mlnToken ethToken
      ]
  runMelonT $ executeSequential initialModelState actions


data ModelState (v :: * -> *) = ModelState
  deriving (Eq, Ord, Show)

initialModelState :: ModelState v
initialModelState = ModelState


-- | Tests share-price invariant
--
-- Invariant of Sum of asset balances times their price (according to price
-- feed) a fund holds divided by the amount of total shares in existence of
-- this fund (= totalSupply) is equal to its sharePrice:
-- @
--   SumForAllAssets(assetBalance * assetPrice) / totalShares == sharePrice
-- @
checkSharePrice
  :: (Monad n, Monad m, MonadCatch m, MonadIO m, MonadTest m, MonadThrow m)
  => Address -- ^ Melon fund
  -> Address -- ^ Price-feed
  -> [Address] -- ^ Owned assets
  -> Command n (MelonT m) ModelState
checkSharePrice fund priceFeed assets =
  let
    gen _ = Just $ pure CheckSharePrice
    execute CheckSharePrice = hoistWeb3 $ do
      defaultCall <- view ctxCall

      let callPriceFeed = defaultCall { callTo = Just priceFeed }
      (prices, _timestamps) <- liftWeb3 $
        CanonicalPriceFeed.getPrices callPriceFeed assets

      balances <- liftWeb3 $ forM assets $ \asset -> do
        let callAsset = defaultCall { callTo = Just asset }
        Asset.balanceOf callAsset fund

      let callFund = defaultCall { callTo = Just fund }
      totalShares <- liftWeb3 $ Fund.totalSupply callFund
      sharePrice <- liftWeb3 $ Fund.calcSharePrice callFund
      decimals <- liftWeb3 $ Fund.getDecimals callFund

      pure (prices, balances, totalShares, sharePrice, decimals)
  in
  Command gen execute
    [ Ensure $ \_prior _after CheckSharePrice
      (prices, balances, totalShares, sharePrice, decimals) -> do
        let precision = 8 :: UIntN 256
            dropDecimals = decimals - precision
            truncate' = (`div` (10^dropDecimals))
        footnote $ "prices " ++ show prices
        footnote $ "balances " ++ show balances
        footnote $ "totalShares " ++ show totalShares
        footnote $ "decimals " ++ show decimals
        footnote $ "dropDecimals " ++ show dropDecimals
        length prices === length balances
        unless (totalShares == 0) $ do
          let calculatedSharePrice =
                sum (zipWith (*) balances prices)
                `div`
                totalShares
          -- Property only holds to a certain precision.
          truncate' sharePrice === truncate' calculatedSharePrice
    ]

data CheckSharePrice (v :: * -> *) = CheckSharePrice
  deriving (Eq, Show)
instance HTraversable CheckSharePrice where
  htraverse _ CheckSharePrice = pure CheckSharePrice


-- | Tests fee-allocation property
--
-- Allocation of management, performance and governance fees to manager does
-- not alter the sharePrice.
--
-- Interpreted as:
-- @
--   sharePrice <- Fund.calcSharePrice
--   _ <- Fund.calcSharePriceAndAllocateFees
--   sharePrice' <- Fund.calcSharePrice
--   sharePrice === sharePrice'
-- @
checkFeeAllocation
  :: (Monad n, Monad m, MonadCatch m, MonadIO m, MonadTest m, MonadThrow m)
  => Address -- ^ Melon fund
  -> Command n (MelonT m) ModelState
checkFeeAllocation fund =
  let
    gen _ = Just $ pure CheckFeeAllocation
    execute CheckFeeAllocation = hoistWeb3 $ do
      defaultCall <- view ctxCall

      let callFund = defaultCall { callTo = Just fund }
      priceBeforeAlloc <- liftWeb3 $ Fund.calcSharePrice callFund
      _ <- liftWeb3 $
        Fund.calcSharePriceAndAllocateFees callFund
        >>= getTransactionEvents >>= \case
          [Fund.CalculationUpdate _ _ _ _ sharePrice _] -> pure sharePrice
          _ -> fail "calcSharePriceAndAllocateFees failed"
      priceAfterAlloc <- liftWeb3 $ Fund.calcSharePrice callFund
      decimals <- liftWeb3 $ Fund.getDecimals callFund

      pure (priceBeforeAlloc, priceAfterAlloc, decimals)
  in
  Command gen execute
    [ Ensure $ \_prior _after CheckFeeAllocation
      (priceBeforeAlloc, priceAfterAlloc, decimals) -> do
        let precision = 8 :: UIntN 256
            dropDecimals = decimals - precision
            truncate' = (`div` (10^dropDecimals))
        footnote $ "decimals " ++ show decimals
        footnote $ "dropDecimals " ++ show dropDecimals
        -- Property only holds to a certain precision.
        -- It does not seem to be influenced by time, this was checked by
        -- introducing an up to one second delay after `priceBeforeAlloc`.
        truncate' priceBeforeAlloc === truncate' priceAfterAlloc
    ]

data CheckFeeAllocation (v :: * -> *) = CheckFeeAllocation
  deriving (Eq, Show)
instance HTraversable CheckFeeAllocation where
  htraverse _ CheckFeeAllocation = pure CheckFeeAllocation


-- | Test make-order share-price property
--
-- Sending of assets to an exchange using a makeOrder does not alter the
-- sharePrice.
checkMakeOrderSharePrice
  :: ( Monad n, MonadGen n, Monad m
     , MonadCatch m, MonadIO m, MonadTest m, MonadThrow m
     )
  => Address -- ^ Melon fund
  -> Address -- ^ Fund manager
  -> MelonT Web3 () -- ^ Update the price feed
  -> Address -- ^ Give asset
  -> Address -- ^ Get asset
  -> Command n (MelonT m) ModelState
checkMakeOrderSharePrice fund manager updatePriceFeed giveAsset getAsset =
  let
    gen _ = Just $ CheckMakeOrderSharePrice
      -- XXX: MatchingMarketAdapter and SimpleAdapter have mismatching signatures.
      --   Indeed this causes the call to SimpleAdapter (index 0) to fail.
      -- XXX: Number of exchanges is hard-coded
      <$> Gen.integral (Range.linear 1 2)
      <*> Gen.integral (Range.linear 1 (10^(23::Int)))
      <*> Gen.integral (Range.linear 1 (10^(23::Int)))
    execute (CheckMakeOrderSharePrice exchangeIndex sellQuantity getQuantity) = do
      defaultCall <- view ctxCall

      let callFund = defaultCall { callTo = Just fund }
      decimals <- evalM $ hoistWeb3 $ liftWeb3 $ Fund.getDecimals callFund
      -- Need to update the price feed here. Otherwise, 'calcSharePrice' might
      -- fail.
      -- XXX: Should we automatically run 'calcSharePrice' every so often?
      evalM $ hoistWeb3 $ updatePriceFeed
      priceBeforeOrder <- evalM $ hoistWeb3 $ liftWeb3 $ Fund.calcSharePrice callFund

      let managerCallFund = callFund { callFrom = Just manager }
          -- XXX: MatchingMarketAdapter and SimpleAdapter have mismatching signatures.
          --   Is that intentioal? Does that mean one of them will not be found?
          makeOrderSignature = encodeSignature (Proxy @MatchingMarketAdapter.MakeOrderData)
          nullAddress = "0x0000000000000000000000000000000000000000"
      (evalM $ hoistWeb3 $ liftWeb3 $
        Fund.callOnExchange managerCallFund
          exchangeIndex
          makeOrderSignature
          -- maker, taker, giveAsset, getAsset, feeRecipient
          [nullAddress, nullAddress, giveAsset, getAsset, nullAddress]
          -- giveQuantity, getQuantity, makerFee, takerFee, timestamp, salt and fillTakerTokenAmount
          [sellQuantity, getQuantity, 0, 0, 0, 0, 0]
          "0x" -- identifier
          0 "0x" "0x" -- v r s
        >>= getTransactionEvents) >>= \case
          [MatchingMarketAdapter.OrderUpdated _exchange _orderId 0] ->
            annotate "makeOrder succeeded"
          _ ->
            -- Make order might fail for all sorts of reasons.
            -- This test-case is not testing that aspect.
            annotate "makeOrder failed"

      priceAfterOrder <- evalM $ hoistWeb3 $ liftWeb3 $ Fund.calcSharePrice callFund

      pure (priceBeforeOrder, priceAfterOrder, decimals)
  in
  Command gen execute
    [ Ensure $ \_prior _after CheckMakeOrderSharePrice {}
      (priceBeforeOrder, priceAfterOrder, decimals) -> do
        let precision = 8 :: UIntN 256
            dropDecimals = decimals - precision
            truncate' = (`div` (10^dropDecimals))
        footnote $ "decimals " ++ show decimals
        footnote $ "dropDecimals " ++ show dropDecimals
        -- Property only holds to a certain precision.
        truncate' priceBeforeOrder === truncate' priceAfterOrder
    ]

data CheckMakeOrderSharePrice (v :: * -> *)
  = CheckMakeOrderSharePrice
      (UIntN 256)  -- ^ Exchange index
      (UIntN 256)  -- ^ Sell quantity
      (UIntN 256)  -- ^ Get quantity
  deriving (Eq, Show)
instance HTraversable CheckMakeOrderSharePrice where
  htraverse _ (CheckMakeOrderSharePrice exchangeIndex sellQuantity getQuantity)
    = CheckMakeOrderSharePrice
    <$> pure exchangeIndex
    <*> pure sellQuantity
    <*> pure getQuantity

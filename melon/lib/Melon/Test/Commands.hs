{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Melon.Test.Commands where

import Control.Exception.Safe (MonadThrow)
import Control.Lens (view)
import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.Ethereum.ABI.Prim.Address (Address)
import Network.Ethereum.ABI.Prim.Int (UIntN)
import Network.Ethereum.Web3.Types (Call (..))

import qualified Melon.ABI.Assets.Asset as Asset
import qualified Melon.ABI.Fund as Fund
import qualified Melon.ABI.PriceFeeds.CanonicalPriceFeed as CanonicalPriceFeed
import Melon.ThirdParty.Network.Ethereum.Web3.Eth (getTransactionEvents)
import Melon.Context
import Melon.Deploy (deploy)


tests :: IO Bool
tests = checkParallel $$discover


prop_melonport :: Property
prop_melonport = withTests 10 $ property $ do
  -- XXX: This repeats the full deployment for each test run.
  --   It might be better (certainly for performance) to only setup a new fund
  --   for each test-case, and share the same 'Version' instance between tests.
  (fund, priceFeed, assets) <- liftIO deploy
  actions <- forAll $
    Gen.sequential (Range.linear 1 100) initialModelState
      [ checkSharePrice fund priceFeed assets
      , checkFeeAllocation fund
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
-- this fund (= totalSupply) is equal to its sharePrice
-- @
--   SumForAllAssets(assetBalance * assetPrice) / totalShares == sharePrice
-- @
checkSharePrice
  :: (Monad n, Monad m, MonadIO m, MonadThrow m)
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
-- not alter the sharePrice
--
-- Interpreted as:
-- @
--   sharePrice <- Fund.calcSharePrice
--   sharePrice' <- Fund.calcSharePriceAndAllocateFees
--   sharePrice === sharePrice'
-- @
checkFeeAllocation
  :: (Monad n, Monad m, MonadIO m, MonadThrow m)
  => Address -- ^ Melon fund
  -> Command n (MelonT m) ModelState
checkFeeAllocation fund =
  let
    gen _ = Just $ pure CheckFeeAllocation
    execute CheckFeeAllocation = hoistWeb3 $ do
      defaultCall <- view ctxCall

      let callFund = defaultCall { callTo = Just fund }
      priceBeforeAlloc <- liftWeb3 $ Fund.calcSharePrice callFund
      priceAfterAlloc <- liftWeb3 $
        Fund.calcSharePriceAndAllocateFees callFund
        >>= getTransactionEvents >>= \case
          [Fund.CalculationUpdate _ _ _ _ sharePrice _] -> pure sharePrice
          _ -> fail "calcSharePriceAndAllocateFees failed"
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
        truncate' priceBeforeAlloc === truncate' priceAfterAlloc
    ]

data CheckFeeAllocation (v :: * -> *) = CheckFeeAllocation
  deriving (Eq, Show)
instance HTraversable CheckFeeAllocation where
  htraverse _ CheckFeeAllocation = pure CheckFeeAllocation

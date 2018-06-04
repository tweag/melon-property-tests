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
import Control.Monad.Trans.Class
import qualified Data.HashMap.Strict as HashMap
import Data.Proxy (Proxy (..))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.Ethereum.ABI.Prim.Address (Address)
import Network.Ethereum.ABI.Prim.Int (UIntN)
import Network.Ethereum.Web3.Eth (accounts)
import Network.Ethereum.Web3.Types (Call (..))
import Network.HTTP.Client (defaultManagerSettings, newManager)

import qualified Melon.ABI.Assets.Asset as Asset
import qualified Melon.ABI.Exchange.Adapter.MatchingMarketAdapter as MatchingMarketAdapter
import qualified Melon.ABI.Fund as Fund
import qualified Melon.ABI.PriceFeeds.CanonicalPriceFeed as CanonicalPriceFeed
import Melon.ThirdParty.Network.Ethereum.ABI.Codec (encodeSignature)
import Melon.ThirdParty.Network.Ethereum.Web3.Eth (getTransactionEvents)
import Melon.Context
import qualified Melon.Contract.Fund as Fund
import qualified Melon.Contract.PriceFeed as PriceFeed
import qualified Melon.Contract.Version as Version
import Melon.Model.Input
import Melon.Model.State


-- | Executes all test-cases.
tests :: IO Bool
tests = do
  manager <- newManager defaultManagerSettings
  provider <- getProvider
  checkParallel $ Group "Melon.Test.Commands"
    [ ("prop_melonport", prop_melonport manager provider) ]


-- | Defines the Melon fund state-machine test.
--
-- First, sets up a version and fund contract with some fixed and some random
-- parameters. Then, generates a random sequence of commands to execute on
-- the fund contract. Finally, executes these commands and checks specified
-- invariants in-between. If a counter-example is discovered, the test-library
-- will "shrink" the random inputs to try and uncover a minimal counter example.
--
-- "Shrinking" generally means to choose smaller values for parameters that
-- where chosen at random. E.g. if, initially, a value of 10^17 was chosen for
-- @managementFee@, then shrinking will check if the invariant still fails for
-- smaller and smaller values of @managementFee@ until it hits the minimum
-- value, zero in this case.
prop_melonport :: Manager -> Provider -> Property
prop_melonport httpManager web3Provider =
  withTests 10 $ property $
  runMelonT httpManager web3Provider $ do

  ----------------------------------------------------------
  -- Determine fees
  let hundredPercent = 10^(18::Int) :: UIntN 256
  managementFee <- lift $ forAll $
    Gen.integral (Range.linear 0 (hundredPercent - 1))
  performanceFee <- lift $ forAll $
    Gen.integral (Range.linear 0 (hundredPercent - 1))

  ----------------------------------------------------------
  -- Setup version and fund
  owner:manager:investors <- liftWeb3 accounts
  version <- Version.deploy owner
  fund <- Fund.deploy version manager managementFee performanceFee

  -- Enable investment and redemption
  -- XXX: Move this into tests and make it part of the model.
  defaultCall <- getCall
  let managerCallFund = defaultCall
        { callFrom = Just $ fund^.fdManager
        , callTo = Just $ fund^.fdAddress
        }
      assets = version^.vdAssets.to HashMap.keys
  void $ liftWeb3 $ Fund.enableInvestment managerCallFund assets
  void $ liftWeb3 $ Fund.enableRedemption managerCallFund assets

  ----------------------------------------------------------
  -- Setup price feed update
  updatePriceFeed <- do
    priceSourceSpec <- lift $ forAll $ genPriceSource version
    priceSource <- liftIO $ PriceFeed.instantiatePriceSource priceSourceSpec
    pure $ PriceFeed.updateCanonicalPriceFeed
      priceSource
      (version^.vdCanonicalPriceFeed)
      (version^.vdStakingPriceFeed)
      (version^.vdOwner)
      (version^.vdAssets.to HashMap.keys)

  ----------------------------------------------------------
  -- Test model input
  let input = ModelInput
        { _miVersion = version
        , _miFund = fund
        , _miUpdatePriceFeed = updatePriceFeed
        , _miInvestors = investors
        }

  ----------------------------------------------------------
  -- Footnotes

  -- Available assets
  -- XXX: Make this prettier.
  footnote $ "Assets:\n" ++ unlines
    [ "  " ++ show address ++ ": " ++ show (asset^.asName)
    | (address, asset) <- input^.miVersion.vdAssets.to HashMap.toList
    ]

  ----------------------------------------------------------
  -- Execute commands

  actions <- lift $ forAll $
    Gen.sequential (Range.linear 1 100) initialModelState
      [ checkSharePrice input
      , checkFeeAllocation input
      , checkMakeOrderSharePrice input
      , checkRequestInvestment input
      ]
  executeSequential initialModelState actions


-- | Randomly generate a price-source specification
genPriceSource :: MonadGen m
  => VersionDeployment -> m PriceFeed.PriceSourceSpec
genPriceSource version = do
  let numAssets = HashMap.size (version^.vdAssets)
      -- Takes a random list of asset prices and cycles through them.
      unrealisticCyclicPriceSource =
        fmap PriceFeed.UnrealisticCyclicPriceSource $
          Gen.list (Range.linear 1 100) $ -- a cycle of up to 100 prices
            Gen.list (Range.singleton numAssets) $ -- one price per asset
              -- XXX:
              --   The CanonicalPriceFeed.collectAndUpdate operation does
              --   not complete on an all zero price update, or an update
              --   of the form @[0, 1, 1]@.
              --   However, the StakingPriceFeed.update operation accepts
              --   an all zero price update.
              -- Gen.integral (Range.linear 0 maxBound)
              Gen.integral (Range.linear 1 maxBound)
      -- Does one lookup on CryptoCompare and then repeats these prices.
      realisticConstantPriceSource = pure $
        PriceFeed.RealisticConstantPriceSource "MLN" 18
          [ (asset^.asCryptoCompareName, asset^.asDecimals)
          | asset <- version^.vdAssets.to HashMap.elems
          ]
  Gen.frequency
    [ (1, unrealisticCyclicPriceSource)
    , (2, realisticConstantPriceSource)
    ]


----------------------------------------------------------------------
-- Commands and Invariants


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
  => ModelInput m
  -> Command n (MelonT m) ModelState
checkSharePrice input =
  let
    updatePriceFeed = input^.miUpdatePriceFeed
    fund = input^.miFund.fdAddress
    priceFeed = input^.miVersion.vdCanonicalPriceFeed
    assets = input^.miVersion.vdAssets.to HashMap.keys

    gen _ = Just $ pure CheckSharePrice
    execute CheckSharePrice = do
      defaultCall <- view ctxCall

      evalM $ updatePriceFeed

      let callPriceFeed = defaultCall { callTo = Just priceFeed }
      (prices, _timestamps) <- evalM $ liftWeb3 $
        CanonicalPriceFeed.getPrices callPriceFeed assets

      balances <- evalM $ liftWeb3 $ forM assets $ \asset -> do
        let callAsset = defaultCall { callTo = Just asset }
        Asset.balanceOf callAsset fund

      let callFund = defaultCall { callTo = Just fund }
      totalShares <- evalM $ liftWeb3 $ Fund.totalSupply callFund
      sharePrice <- evalM $ liftWeb3 $ Fund.calcSharePrice callFund
      decimals <- evalM $ liftWeb3 $ Fund.getDecimals callFund

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
  => ModelInput m
  -> Command n (MelonT m) ModelState
checkFeeAllocation input =
  let
    updatePriceFeed = input^.miUpdatePriceFeed
    fund = input^.miFund.fdAddress

    gen _ = Just $ pure CheckFeeAllocation
    execute CheckFeeAllocation = do
      defaultCall <- view ctxCall

      evalM updatePriceFeed

      let callFund = defaultCall { callTo = Just fund }
      priceBeforeAlloc <- evalM $ liftWeb3 $ Fund.calcSharePrice callFund
      _ <- evalM $ liftWeb3 $
        Fund.calcSharePriceAndAllocateFees callFund
        >>= getTransactionEvents >>= \case
          [Fund.CalculationUpdate _ _ _ _ sharePrice _] -> pure sharePrice
          _ -> fail "calcSharePriceAndAllocateFees failed"
      priceAfterAlloc <- evalM $ liftWeb3 $ Fund.calcSharePrice callFund
      decimals <- evalM $ liftWeb3 $ Fund.getDecimals callFund

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
  => ModelInput m
  -> Command n (MelonT m) ModelState
checkMakeOrderSharePrice input =
  let
    fund = input^.miFund.fdAddress
    manager = input^.miFund.fdManager
    updatePriceFeed = input^.miUpdatePriceFeed
    -- XXX: Could we pick these at random?
    giveAsset = input^.miVersion.vdMlnToken
    getAsset = input^.miVersion.vdEthToken

    gen _ = Just $ CheckMakeOrderSharePrice
      -- XXX: MatchingMarketAdapter and SimpleAdapter have mismatching signatures.
      --   Indeed this causes the call to SimpleAdapter (index 0) to fail.
      -- XXX: Number of exchanges is hard-coded
      <$> Gen.integral (Range.linear 1 2)
      <*> Gen.integral (Range.linear 1 (10^(23::Int)))
      <*> Gen.integral (Range.linear 1 (10^(23::Int)))
    execute (CheckMakeOrderSharePrice exchangeIndex sellQuantity getQuantity) = do
      defaultCall <- getCall

      let callFund = defaultCall { callTo = Just fund }
      decimals <- evalM $ liftWeb3 $ Fund.getDecimals callFund
      -- Need to update the price feed here. Otherwise, 'calcSharePrice' might
      -- fail.
      -- XXX: Should we automatically run 'calcSharePrice' every so often?
      evalM $ updatePriceFeed
      priceBeforeOrder <- evalM $ liftWeb3 $ Fund.calcSharePrice callFund

      let managerCallFund = callFund { callFrom = Just manager }
          -- XXX: MatchingMarketAdapter and SimpleAdapter have mismatching signatures.
          --   Is that intentioal? Does that mean one of them will not be found?
          makeOrderSignature = encodeSignature (Proxy @MatchingMarketAdapter.MakeOrderData)
          nullAddress = "0x0000000000000000000000000000000000000000"
      evalM $ (liftWeb3 $
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

      priceAfterOrder <- evalM $ liftWeb3 $ Fund.calcSharePrice callFund

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


-- | Test request-investment property
--
-- - Investment and redemption by regular means (request and execute, not
--     @emergencyRedeem@) do not immediately alter share price
checkRequestInvestment
  :: ( Monad n, MonadGen n, Monad m
     , MonadCatch m, MonadIO m, MonadTest m, MonadThrow m
     )
  => ModelInput m
  -> Command n (MelonT m) ModelState
checkRequestInvestment input =
  let
    updatePriceFeed = input^.miUpdatePriceFeed
    fund = input^.miFund.fdAddress
    owner = input^.miVersion.vdOwner

    gen _ = Just $ CheckRequestInvestment
      <$> Gen.element (input^.miInvestors)
      <*> Gen.element (input^.miVersion.vdAssets.to HashMap.keys)
      <*> Gen.integral (Range.linear 1 100000)
      <*> Gen.integral (Range.linear 1 100000)
    execute (CheckRequestInvestment investor token give share) = do
      defaultCall <- getCall
      let callFund = defaultCall { callTo = Just fund }
          ownerCallToken = defaultCall
            { callFrom = Just owner
            , callTo = Just token
            }
          investorCallToken = defaultCall
            { callFrom = Just investor
            , callTo = Just token
            }
          investorCallFund = defaultCall
            { callFrom = Just investor
            , callTo = Just fund
            }

      -- Price-feed must be up-to-date
      evalM updatePriceFeed

      -- Share-price before
      priceBefore <- evalM $ liftWeb3 $ Fund.calcSharePrice callFund

      -- Transfer the amount
      -- XXX: Take previous transfers into account
      evalM $ liftWeb3 $
        Asset.transfer ownerCallToken investor give
        >>= getTransactionEvents >>= \case
          [Asset.Transfer {}] -> pure ()
          _ -> fail "Token transfer failed."

      -- Approve the amount
      -- XXX: Take previous approval into account
      evalM $ liftWeb3 $
        Asset.approve investorCallToken fund give
        >>= getTransactionEvents >>= \case
          [Asset.Approval {}] -> pure ()
          _ -> fail "Investor token approval failed."

      -- Request investment
      -- XXX: Keep track of open investment requests
      _reqId <- evalM $ liftWeb3 $
        Fund.requestInvestment investorCallFund give share token
        >>= getTransactionEvents >>= \case
          [Fund.RequestUpdated reqId] -> pure reqId
          _ -> fail "Investment request failed."

      -- Share-price after investment
      priceAfter <- evalM $ liftWeb3 $ Fund.calcSharePrice callFund

      pure (priceBefore, priceAfter)
  in
  Command gen execute
    [ Ensure $ \_prior _after CheckRequestInvestment {}
      (priceBefore, priceAfter) -> do
        -- Property holds exactly.
        priceBefore === priceAfter
    ]

data CheckRequestInvestment (v :: * -> *)
  = CheckRequestInvestment
      Address -- ^ Investor
      Address -- ^ Trade token
      (UIntN 256) -- ^ Give quantity
      (UIntN 256) -- ^ Share quantity
  deriving (Eq, Show)
instance HTraversable CheckRequestInvestment where
  htraverse _ (CheckRequestInvestment investor token give share)
    = CheckRequestInvestment
    <$> pure investor
    <*> pure token
    <*> pure give
    <*> pure share

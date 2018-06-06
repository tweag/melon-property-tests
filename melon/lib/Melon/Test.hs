{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Melon.Test where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class
import Data.ByteArray (convert)
import qualified Data.ByteString.Char8 as C8
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.Ethereum.ABI.Prim.Int (UIntN)
import Network.Ethereum.Web3.Eth (accounts)
import Network.Ethereum.Web3.Types (Call (..))
import Network.HTTP.Client (defaultManagerSettings, newManager)

import qualified Melon.ABI.Fund as Fund
import Melon.Context
import qualified Melon.Contract.Fund as Fund
import qualified Melon.Contract.PriceFeed as PriceFeed
import qualified Melon.Contract.Version as Version
import Melon.Model.Input
import Melon.Model.State
import Melon.Test.Commands


-- | Executes all test-cases.
tests :: IO Bool
tests = do
  manager <- newManager defaultManagerSettings
  provider <- getProvider
  checkSequential $ Group "Melon.Test.Commands"
    [ ("prop_melonport_model", prop_melonport_model 20 200 manager provider) ]


-- | Recheck a particular test-case.
--
-- If a test-case fails and the message says some thing along the lines of
--
--    > recheck (Size 8) (Seed 9082926469563838346 (-5629536107532025561)) prop_melonport_model
--
-- Then you can perform that re-check by executing
--
--     recheck_prop_melonport (Size 8) (Seed 908... (-562...))
--
recheck_prop_melonport :: Size -> Seed -> IO ()
recheck_prop_melonport size seed = do
  manager <- newManager defaultManagerSettings
  provider <- getProvider
  recheck size seed (prop_melonport_model 20 200 manager provider)


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
prop_melonport_model :: TestLimit -> Int -> Manager -> Provider -> Property
prop_melonport_model numTests numCommands httpManager web3Provider =
  withTests numTests $
  withShrinks 0 $
  withRetries 0 $
  property $
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
  -- XXX:
  --   For now we just enable investment/redemeption of all assets.
  --   This should be captured by the model state and enabling/disabling
  --   investment/redemption should be made into commands.
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
  (prices, priceFeed) <- do
    priceFeedSpec <- lift $ forAll $ genPriceFeedSpec version
    prices:priceFeed <- liftIO $ PriceFeed.createPriceFeed priceFeedSpec
    -- Perform one initial price-feed update.
    PriceFeed.updatePriceFeed version prices
    pure (prices, priceFeed)

  ----------------------------------------------------------
  -- Test model
  let input = ModelInput
        { _miVersion = version
        , _miFund = fund
        , _miInvestors = investors
        }
      state = ModelState
        { _msQuoteAsset = version^.vdMlnToken
        , _msQuoteDecimals = fromMaybe 0 $
            version^?vdAssets.ix (version^.vdMlnToken).asDecimals.to fromIntegral
        , _msDecimals = fmap (fromIntegral . _asDecimals) $ version^.vdAssets
        , _msPriceUpdateId = 1
        , _msPrices = prices
        , _msPriceFeed = priceFeed
        , _msTotalShares = 0
        , _msAssetBalances = mempty
        , _msIsShutdown = False
        , _msInvestments = []
        , _msInvestorAssets = mempty
        }

  ----------------------------------------------------------
  -- Footnotes

  -- Accounts
  footnote $ "Owner: " ++ show owner
  footnote $ "Manager: " ++ show manager
  footnote $ "Investors:\n" ++ unlines
    [ "  " ++ show investor | investor <- investors ]

  -- Contracts
  footnote $ "Version: " ++ show (version^.vdAddress)
  footnote $ "Fund: " ++ show (fund^.fdAddress)

  -- Assets
  footnote $ "Assets:\n" ++ unlines
    [ "  "
      ++ (C8.unpack . convert $ asset^.asName)
      ++ ": " ++ show address
    | (address, asset) <- input^.miVersion.vdAssets.to HashMap.toList
    ]

  ----------------------------------------------------------
  -- Execute commands

  actions <- lift $ forAll $
    Gen.sequential (Range.linear 1 numCommands) state
      [ cmdUpdatePriceFeed input
      , checkSharePrice input
      , requestValidInvestment input
      , executeValidInvestment input
      -- , checkIsFundShutdown input
      -- , managerCanShutdownFund input
      -- , checkFeeAllocation input
      -- , checkMakeOrderSharePrice input
      -- , checkRequestInvestment input
      ]
  executeSequential state actions


-- | Randomly generate a price-source specification
genPriceFeedSpec :: MonadGen m
  => VersionDeployment -> m PriceFeed.PriceFeedSpec
genPriceFeedSpec version = do
  let assets = version^.vdAssets.to HashMap.keys
      numAssets = length assets
      -- Takes a random list of asset prices and cycles through them.
      -- unrealisticCyclicPriceSource maxPrice =
      --   fmap PriceFeed.UnrealisticCyclicPriceFeed $
      --     Gen.list (Range.linear 1 100) $ -- a cycle of up to 100 prices
      --       fmap (HashMap.fromList . zip assets) $
      --       Gen.list (Range.singleton numAssets) $ -- one price per asset
      --         -- XXX:
      --         --   The CanonicalPriceFeed.collectAndUpdate operation does
      --         --   not complete on an all zero price update, or an update
      --         --   of the form @[0, 1, 1]@.
      --         --   However, the StakingPriceFeed.update operation accepts
      --         --   an all zero price update.
      --         Gen.integral (Range.linear 1 maxPrice)
      -- Does one lookup on CryptoCompare and then repeats these prices.
      realisticConstantPriceSource = pure $
        PriceFeed.RealisticConstantPriceFeed "MLN" 18 (version^.vdAssets)
      -- Does one lookup on CryptoCompare and generates a cycle of slight
      -- variations of these prices that will be repeated indefinitely.
      realisticCyclicPriceSource = do
        variations <-
          Gen.list (Range.linear 1 100) $
            fmap (HashMap.fromList . zip assets) $
            Gen.list (Range.singleton numAssets) $ -- one variation per asset
              Gen.double (Range.linearFracFrom 0 (-0.05) 0.05) -- up to 5% variations per step
        pure $ PriceFeed.RealisticCyclicPriceFeed
          "MLN" 18 (version^.vdAssets) variations
  Gen.frequency
    [ (1, realisticCyclicPriceSource)
    -- XXX:
    --   Strong fluctuations in the price-feed can amplify the rounding issues
    --   within the Melon func contract and cause differences between expected
    --   and observed share-price on the order of 10%.
    -- , (1, unrealisticCyclicPriceSource (10^(30::Int)))
    -- XXX:
    --   Too large prices cause overflow in the middle of the models
    --   calculations. A general handling of that would be good, but for now
    --   the price-feed is kept more sensible.
    -- , (1, unrealisticCyclicPriceSource maxBound)
    , (2, realisticConstantPriceSource)
    ]

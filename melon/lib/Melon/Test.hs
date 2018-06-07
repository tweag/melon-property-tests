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
--
--
tests :: TestLimit -> Int -> IO Bool
tests numTests numCommands = do
  manager <- newManager defaultManagerSettings
  provider <- getProvider
  checkSequential $ Group "Melon.Test"
    [ ("prop_melonport", prop_melonport numTests numCommands manager provider)
    , ("prop_melonport_model", prop_melonport_model numTests numCommands manager provider)
    ]


-- | Recheck a particular test-case.
--
-- If a test-case fails and the message says some thing along the lines of
--
--    > recheck (Size 8) (Seed 9082926469563838346 (-5629536107532025561)) prop_melonport
--
-- Then you can perform that re-check by executing
--
--     recheck_prop_melonport (Size 8) (Seed 908... (-562...))
--
recheck_prop_melonport :: Size -> Seed -> IO ()
recheck_prop_melonport size seed = do
  manager <- newManager defaultManagerSettings
  provider <- getProvider
  recheck size seed (prop_melonport 20 200 manager provider)


-- | Recheck a particular test-case.
--
-- If a test-case fails and the message says some thing along the lines of
--
--    > recheck (Size 8) (Seed 9082926469563838346 (-5629536107532025561)) prop_melonport_model
--
-- Then you can perform that re-check by executing
--
--     recheck_prop_melonport_model (Size 8) (Seed 908... (-562...))
--
recheck_prop_melonport_model :: Size -> Seed -> IO ()
recheck_prop_melonport_model size seed = do
  manager <- newManager defaultManagerSettings
  provider <- getProvider
  recheck size seed (prop_melonport_model 20 200 manager provider)


-- | Simple Melon fund state-machine test.
--
-- First, sets up a version and fund contract with some fixed and some random
-- parameters. Then, generates a random sequence of commands to execute on
-- the fund contract. Finally, executes these commands and checks specified
-- invariants in-between.
--
-- These tests do not carry a model of the fund contract. Instead, where
-- required the current state of the contract is simply queried before
-- performing an operation.
--
-- The advantage is that more operations can be encoded without the need to
-- develop a precise model of all the contract components. E.g. time-based
-- effects like the management fees can be incorporated in the tests this way
-- without the need to fully model their effects.
prop_melonport :: TestLimit -> Int -> Manager -> Provider -> Property
prop_melonport numTests numCommands httpManager web3Provider =
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
  (_prices, priceFeed) <- do
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
      state = SimpleModelState
        { _smsPriceFeed = priceFeed
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
      [ checkFeeAllocation input
      , checkMakeOrder input
      , checkRequestInvestment input
      ]
  executeSequential state actions


-- | Modelled Melon fund state-machine test.
--
-- First, sets up a version and fund contract with some fixed and some random
-- parameters. Then, generates a random sequence of commands to execute on
-- the fund contract. Finally, executes these commands and checks specified
-- invariants in-between.
--
-- These tests carry a model of the fund contract. This allows for more precise
-- tests, e.g. expecting that certain investments should always succeed.
-- On the other hand, certain aspects cannot be modelled properly without
-- mocking the external effects that they depend on. Time-based effects like
-- the management fees would best be modelled with a mocked time-source where
-- the tests could explicitely tell parity when to advance the clock.
prop_melonport_model :: TestLimit -> Int -> Manager -> Provider -> Property
prop_melonport_model numTests numCommands httpManager web3Provider =
  withTests numTests $
  withShrinks 0 $
  withRetries 0 $
  property $
  runMelonT httpManager web3Provider $ do

  ----------------------------------------------------------
  -- Determine fees
  let -- Fixed to zero as time-based effects are not modelled at the moment.
      -- These are tested at non-zero values in the simple tests.
      managementFee = 0
      -- XXX: Incorporate markets into the model
      --   These are tested at non-zero values in the simple tests.
      performanceFee = 0

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
      , checkIsFundShutdown input
      , managerCanShutdownFund input
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
      --         -- FAILURE:
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
    [ (1, realisticConstantPriceSource)
    , (2, realisticCyclicPriceSource)
    -- FAILURE:
    --   Strong fluctuations in the price-feed can amplify the rounding issues
    --   within the Melon func contract and cause differences between expected
    --   and observed share-price on the order of 10%.
    -- , (1, unrealisticCyclicPriceSource (10^(30::Int)))
    --
    -- XXX:
    --   The model needs to be extended to detect uint256 overflow to be able
    --   to predict when e.g. a investment will fail due to overflow.
    --   Currently, this leads to discrepancy between the contract and the
    --   model and consequently to test-case failure. A dedicated number type
    --   could be a good approach. E.g.
    --
    --     -- | Unsigned fixed point decimal number with fixed amount of bits.
    --     data UDecimal (bits :: Nat) (decimals :: Nat)
    --       = UOverflow -- ^ The number is the result of an overflow.
    --       | UValid (UIntN bits) -- ^ The number is valid.
    --
    --   The Num instances could then keep track of overflows.
    --
    --   For asset prices at different decimals we could have an existential
    --   wrapper where the Num instances converts to common decimal precision
    --   before performing operations and also keeps track of overflows.
    --
    -- , (1, unrealisticCyclicPriceSource maxBound)
    ]

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Melon.Test.Commands where

import Control.Exception.Safe (MonadCatch, MonadThrow)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class
import Data.ByteArray (convert)
import qualified Data.ByteString.Char8 as C8
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
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
import qualified Melon.ABI.Version.Version as Version
import Melon.ThirdParty.Network.Ethereum.ABI.Codec (encodeCall, encodeSignature)
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
  checkSequential $ Group "Melon.Test.Commands"
    [ ("prop_melonport", prop_melonport 20 200 manager provider) ]


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


-- | @truncateTo requestedPrecision givenDecimals givenValue@.
truncateTo :: (Integral d, Integral v) => d -> d -> v -> v
truncateTo precision decimals value = value `div` (10^(decimals - precision))


----------------------------------------------------------------------
-- Commands and Invariants
----------------------------------------------------------------------

------------------------------------------------------------
-- Fund shutdown

-- | Check if the fund is shut-down.
data CmdIsFundShutdown (v :: * -> *)
  = CmdIsFundShutdown
  deriving (Eq, Show)
instance HTraversable CmdIsFundShutdown where
  htraverse _ _ = pure CmdIsFundShutdown

-- | Check that the model and fund agree on shut-down status.
checkIsFundShutdown
  :: (Monad n, Monad m, MonadCatch m, MonadIO m, MonadTest m, MonadThrow m)
  => ModelInput
  -> Command n (MelonT m) ModelState
checkIsFundShutdown input =
  let
    fund = input^.miFund.fdAddress

    gen _ = Just $ pure CmdIsFundShutdown
    execute CmdIsFundShutdown = do
      defaultCall <- getCall
      let callFund = defaultCall { callTo = Just fund }
      evalM $ liftWeb3 $ Fund.isShutDown callFund
  in
  Command gen execute
    [ Ensure $ \s _ _ o -> s^.msIsShutdown === o ]

-- | @CmdShutdownFund account@
--
-- The given @account@ will attempt to shut-down the fund.
newtype CmdShutdownFund (v :: * -> *)
  = CmdShutdownFund Address
  deriving (Eq, Show)
instance HTraversable CmdShutdownFund where
  htraverse _ (CmdShutdownFund account) =
    pure $ CmdShutdownFund account

-- | Manager can shut-down the fund contract
managerCanShutdownFund
  :: (Monad n, Monad m, MonadCatch m, MonadIO m, MonadTest m, MonadThrow m)
  => ModelInput
  -> Command n (MelonT m) ModelState
managerCanShutdownFund input =
  let
    version = input^.miVersion.vdAddress
    fund = input^.miFund.fdAddress

    gen s
      | s^.msIsShutdown = Nothing -- Already shut-down
      | otherwise       = Just $ pure $
          CmdShutdownFund (input^.miFund.fdManager)
    execute (CmdShutdownFund manager) = do
      defaultCall <- getCall
      let managerCallVersion = defaultCall
            { callFrom = Just manager
            , callTo = Just version
            }
          callFund = defaultCall { callTo = Just fund }
      annotateShow $ encodeCall Fund.ShutDownData
      void $ evalM $ liftWeb3 $ Version.shutDownFund managerCallVersion fund
      evalM $ liftWeb3 $ Fund.isShutDown callFund
  in
  Command gen execute
    [ Require $ \s _ -> not $ s^.msIsShutdown
    , Update $ \s _ _ -> s & msIsShutdown .~ True
    , Ensure $ \_ _ _ o -> o === True
    ]

-- | No one but the manager can shut-down the fund contract
--
-- XXX: This test-case is disabled because it causes timeouts
--   with hs-web3 and parity.
-- onlyManagerCanShutdownFund
--   :: (Monad n, MonadGen n, Monad m, MonadCatch m, MonadIO m, MonadTest m, MonadThrow m)
--   => ModelInput m
--   -> Command n (MelonT m) ModelState
-- onlyManagerCanShutdownFund input =
--   let
--     owner = input^.miVersion.vdOwner
--     version = input^.miVersion.vdAddress
--     fund = input^.miFund.fdAddress
--
--     gen s
--       | s^.msIsShutdown = Nothing -- Already shut-down
--       | otherwise       = Just $ fmap CmdShutdownFund $ Gen.element $
--           owner:version:fund:(input^.miInvestors)
--     execute (CmdShutdownFund account) = do
--       defaultCall <- getCall
--       let accountCallVersion = defaultCall
--             { callFrom = Just account
--             , callTo = Just version
--             }
--           callFund = defaultCall { callTo = Just fund }
--       annotateShow $ encodeCall $ Fund.ShutDownData
--       void $ evalM $ liftWeb3 $ Version.shutDownFund accountCallVersion fund
--       evalM $ liftWeb3 $ Fund.isShutDown callFund
--   in
--   Command gen execute
--     [ Require $ \s _ -> not $ s^.msIsShutdown
--     , Ensure $ \_ _ _ o -> o === False
--     ]


------------------------------------------------------------
-- Investment

-- | An investment request command.
data CmdRequestInvestment (v :: * -> *)
  = CmdRequestInvestment
      !Address -- ^ Investor
      !Address -- ^ Asset
      !(UIntN 256) -- ^ Give amount
      !(UIntN 256) -- ^ Share amount
  deriving (Eq, Show)
instance HTraversable CmdRequestInvestment where
  htraverse _ (CmdRequestInvestment investor asset give share)
    = pure $ CmdRequestInvestment investor asset give share

-- | Request a valid investment.
--
-- Choose an existing investor and asset and request an investment with give
-- and share amounts, such that the investor will be able to afford it and the
-- give amount is compatible to the share price. The give amount will be
-- transferred to the investor and approved for fund investment so that the
-- investor can afford the investment.
--
-- XXX: This test-case assumes that investment is always allowed.
--   Currently, investment requests will always be allowed as long as
--   investment in the particular asset is allowed. This will change once
--   compliance is implemented.
requestValidInvestment
  :: (Monad n, MonadGen n, MonadCatch m, MonadIO m, MonadTest m, MonadThrow m)
  => ModelInput
  -> Command n (MelonT m) ModelState
requestValidInvestment input =
  let
    fund = input^.miFund.fdAddress
    quoteAsset = input^.miVersion.vdMlnToken

    gen s
      | s^.msIsShutdown = Nothing
      | otherwise = Just $ do
          investor <- Gen.element (input^.miInvestors)
          asset <- Gen.element (input^.miVersion.vdAssets.to HashMap.keys)
          -- The premined assets hold 10^28 in supply. Here we stay well below
          -- that amount and assume that the supply is endless.

          let quoteDecimals = s^.msQuoteDecimals
              assetDecimals = s^.msAssetDecimals asset
              sharePrice = s^.msSharePrice
              invAssetPrice = s^.msInvertedAssetPrice asset

          -- We first choose shares for what cost we want to buy.
          cost <- Gen.integral (Range.linear 0 (10^(23::Int)))
          -- Then we determine how many shares it buys us.
          share <-
            if sharePrice == 0 || invAssetPrice == 0 then
              -- If shares are free we can buy arbitrarily many.
              Gen.integral (Range.linear 0 maxBound)
            else if asset /= quoteAsset then
              -- Note, this shouldn't overflow on UIntN 256.
              pure $
                (cost * (10^quoteDecimals) * (10^assetDecimals))
                `div` sharePrice
                `div` invAssetPrice
            else
              -- Note, this shouldn't overflow on UIntN 256.
              pure $
                (cost * (10^quoteDecimals))
                `div` sharePrice
          let actualCost = s^.msShareCost share asset assetDecimals
          -- Give more than it costs.
          give <- Gen.integral (Range.linear actualCost (10^(24::Int)))
          pure $ CmdRequestInvestment investor asset give share
    execute (CmdRequestInvestment investor asset give share) = do
      defaultCall <- getCall
      let investorCallFund = defaultCall
            { callFrom = Just investor
            , callTo = Just fund
            }
      evalM $ liftWeb3 $
        Fund.requestInvestment investorCallFund give share asset
        >>= getTransactionEvents >>= \case
          [Fund.RequestUpdated  reqId] -> pure reqId
          _ -> fail "Failed to request investment."
  in
  Command gen execute
    [ Require $ \s _ -> not $ s^.msIsShutdown
    , Update $ \s (CmdRequestInvestment investor asset give share) o ->
        let request = InvestmentRequest
              { _irId = o
              , _irInvestor = investor
              , _irAsset = asset
              , _irGive = give
              , _irShare = share
              , _irPriceUpdateId = s^.msPriceUpdateId
              }
        in
        s & msInvestments %~ (request:)
    ]


newtype CmdExecuteInvestment (v :: * -> *)
  = CmdExecuteInvestment
      (InvestmentRequest v) -- ^ The investment request
  deriving (Eq, Show)
instance HTraversable CmdExecuteInvestment where
  htraverse f (CmdExecuteInvestment req) = CmdExecuteInvestment
      <$> htraverse f req

executeValidInvestment
  :: (Monad n, MonadGen n, MonadCatch m, MonadIO m, MonadTest m, MonadThrow m)
  => ModelInput
  -> Command n (MelonT m) ModelState
executeValidInvestment input =
  let
    owner = input^.miVersion.vdOwner
    fund = input^.miFund.fdAddress
    cost :: ModelState v -> InvestmentRequest v -> UIntN 256
    cost s investment = s^.msShareCost share asset decimals
      where
        share = investment^.irShare
        asset = investment^.irAsset
        decimals = fromMaybe 0 $
          input^?miVersion.vdAssets.ix asset.asDecimals.to fromIntegral
    isValidInvestment s investment =
      (cost s investment <= (investment^.irGive))
      &&
      ((s^.msPriceUpdateId) >= (investment^.irPriceUpdateId) + 2)

    gen s
      | s^.msIsShutdown = Nothing
      | otherwise =
          let validInvestments = (s^.msInvestments)
                & filter (isValidInvestment s)
          in
          case validInvestments of
            [] -> Nothing
            is -> Just $
              CmdExecuteInvestment <$> Gen.element is
    execute (CmdExecuteInvestment req) = do
      defaultCall <- getCall
      let asset = req^.irAsset
          investor = req^.irInvestor
          give = req^.irGive
          callAsset = defaultCall { callTo = Just asset }
          callFund = defaultCall { callTo = Just fund }
          ownerCallAsset = callAsset { callFrom = Just owner }
          investorCallAsset = callAsset { callFrom = Just investor }
          investorCallFund = callFund { callFrom = Just investor }
      -- Useful annotation for debugging
      -- annotate $ "Investment:\n"
      --   ++ "  investor: " ++ show (req^.irInvestor) ++ "\n"
      --   ++ "  asset: " ++ show (req^.irAsset) ++ "\n"
      --   ++ "  give: " ++ show (req^.irGive) ++ "\n"
      --   ++ "  share: " ++ show (req^.irShare) ++ "\n"
      --   ++ "  price-update: " ++ show (req^.irPriceUpdateId) ++ "\n"
      -- Owner transfers the required amount to the investor.
      evalM $ liftWeb3 $
        Asset.transfer ownerCallAsset investor give
        >>= getTransactionEvents >>= \case
          [Asset.Transfer {}] -> pure ()
          _ -> fail "Failed to transfer give amount to investor."
      -- Investor allows transfer to fund.
      evalM $ liftWeb3 $
        Asset.approve investorCallAsset fund give
        >>= getTransactionEvents >>= \case
          [Asset.Approval {}] -> pure ()
          _ -> fail "Failed to approve give amount for fund."
      -- Useful annotation for debugging
      -- do
      --   balance <- evalM $ liftWeb3 $ Asset.balanceOf callAsset fund
      --   annotate $ "Fund balance before: " ++ show balance
      --   totalSupply <- evalM $ liftWeb3 $ Fund.totalSupply callFund
      --   annotate $ "Total supply before: " ++ show totalSupply
      --   investorBalance <- evalM $ liftWeb3 $ Fund.balanceOf callFund investor
      --   annotate $ "Investor balance before: " ++ show investorBalance
      tx <- evalM $ liftWeb3 $
        Fund.executeRequest investorCallFund (req^.irId.to concrete)
      -- Useful annotation for debugging
      -- do
      --   balance <- evalM $ liftWeb3 $ Asset.balanceOf callAsset fund
      --   annotate $ "Fund balance after: " ++ show balance
      --   totalSupply <- evalM $ liftWeb3 $ Fund.totalSupply callFund
      --   annotate $ "Total supply after: " ++ show totalSupply
      --   investorBalance <- evalM $ liftWeb3 $ Fund.balanceOf callFund investor
      --   annotate $ "Investor balance after: " ++ show investorBalance
      evalM $ liftWeb3 $
        getTransactionEvents tx >>= \case
          [Asset.Transfer {}] -> pure ()
          _ -> fail "Failed to transfer assets on investment."
      -- XXX: Created events are not always fired,
      --   even if the shares are created.
      -- evalM $ liftWeb3 $
      --   getTransactionEvents tx >>= \case
      --     [Fund.Created {}] -> pure ()
      --     _ -> fail "Failed to create shares on investment."
  in
  Command gen execute
    [ Require $ \s _ -> not $ s^.msIsShutdown
    , Require $ \s (CmdExecuteInvestment req) -> isValidInvestment s req
    , Update $ \s (CmdExecuteInvestment req) _ -> s
        & msInvestments %~ filter (\req' -> req'^.irId /= req^.irId)
        & msAssetBalances.at (req^.irAsset).non 0 %~ (+ (cost s req))
        & msTotalShares %~ (+ (req^.irShare))
        -- XXX: Keep per investor share balance.
        -- XXX: This executes calcSharePriceAndAllocateFees.
    ]


------------------------------------------------------------
-- Price Feed

-- | Update the price feed
newtype CmdUpdatePriceFeed (v :: * -> *)
  = CmdUpdatePriceFeed
      (HashMap Address (UIntN 256)) -- ^ The list of new prices.
  deriving (Eq, Show)
instance HTraversable CmdUpdatePriceFeed where
  htraverse _ (CmdUpdatePriceFeed prices) = pure $
    CmdUpdatePriceFeed prices

cmdUpdatePriceFeed
  :: (Monad n, Monad m, MonadCatch m, MonadIO m, MonadTest m, MonadThrow m)
  => ModelInput
  -> Command n (MelonT m) ModelState
cmdUpdatePriceFeed input =
  let
    updatePriceFeed = PriceFeed.updatePriceFeed (input^.miVersion)
    canonicalPriceFeed = input^.miVersion.vdCanonicalPriceFeed

    gen s
      | s^.msIsShutdown = Nothing
      | otherwise = Just $ pure $
          CmdUpdatePriceFeed $ head (s^.msPriceFeed)
    execute (CmdUpdatePriceFeed prices) = do
      updatePriceFeed prices
      defaultCall <- getCall
      let callFeed = defaultCall { callTo = Just canonicalPriceFeed }
          assets = input^.miVersion.vdAssets.to HashMap.keys
      evalM $ liftWeb3 $ CanonicalPriceFeed.getPrices callFeed assets
  in
  Command gen execute
    [ Require $ \s _ -> not $ s^.msIsShutdown
    , Update $ \s (CmdUpdatePriceFeed prices) _ -> s
        & msPrices .~ prices
        & msPriceFeed %~ tail
        & msPriceUpdateId %~ succ
    , Ensure $ \_ _ (CmdUpdatePriceFeed prices) (prices', _) ->
        prices' === HashMap.elems prices
    ]


------------------------------------------------------------
-- Share Price

data CalcSharePrice (v :: * -> *)
  = CalcSharePrice
  deriving (Eq, Show)
instance HTraversable CalcSharePrice where
  htraverse _ _ = pure CalcSharePrice

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
  => ModelInput
  -> Command n (MelonT m) ModelState
checkSharePrice input =
  let
    fund = input^.miFund.fdAddress

    gen _ = Just $ pure CalcSharePrice
    execute CalcSharePrice = do
      defaultCall <- getCall
      let callFund = defaultCall { callTo = Just fund }
      -- Useful annotation for debugging
      -- do
      --   let assets = input^.miVersion.vdAssets.to HashMap.keys
      --   (prices, _) <- evalM $ liftWeb3 $
      --     CanonicalPriceFeed.getPrices defaultCall
      --       { callTo = Just (input^.miVersion.vdCanonicalPriceFeed) }
      --       assets
      --   annotate $ "Asset prices: " ++ show (HashMap.fromList (zip assets prices))
      --   invertedPriceInfos <- evalM $ liftWeb3 $
      --     iforM (input^.miVersion.vdAssets) $ \asset _ -> do
      --       let callPriceFeed = defaultCall { callTo = Just $ input^.miVersion.vdCanonicalPriceFeed }
      --       CanonicalPriceFeed.getInvertedPriceInfo callPriceFeed asset
      --   annotate $ "Inverted price info: " ++ show invertedPriceInfos
      --   balances <- evalM $ liftWeb3 $
      --     iforM (input^.miVersion.vdAssets) $ \asset _ -> do
      --       Asset.balanceOf defaultCall { callTo = Just asset } fund
      --   annotate $ "Fund's asset balances: " ++ show balances
      --   totalSupply <- evalM $ liftWeb3 $ Fund.totalSupply callFund
      --   annotate $ "Total supply: " ++ show totalSupply
      evalM $ liftWeb3 $ Fund.calcSharePrice callFund
  in
  Command gen execute
    [ Ensure $ \s _ _ sharePrice -> do
        annotate $ "Asset prices:\n" ++ unlines
          [ "  " ++ show asset ++ ": " ++ show price
          | (asset, price) <- s^.msPrices.to HashMap.toList ]
        annotate $ "Asset balances:\n" ++ unlines
          [ "  " ++ show asset ++ ": " ++ show balance
          | (asset, balance) <- s^.msAssetBalances.to HashMap.toList ]
        annotate $ "Total shares: " ++ show (s^.msTotalShares)
        annotate $ "Expected share price: " ++ show (s^.msSharePrice)
        annotate $ "Actual share price: " ++ show sharePrice
        let truncate' = truncateTo 8 (s^.msQuoteDecimals)
        truncate' sharePrice === truncate' (s^.msSharePrice)
    ]

-- | Tests share-price invariant
--
-- Invariant of Sum of asset balances times their price (according to price
-- feed) a fund holds divided by the amount of total shares in existence of
-- this fund (= totalSupply) is equal to its sharePrice:
-- @
--   SumForAllAssets(assetBalance * assetPrice) / totalShares == sharePrice
-- @
checkSharePriceOld
  :: (Monad n, Monad m, MonadCatch m, MonadIO m, MonadTest m, MonadThrow m)
  => ModelInput
  -> Command n (MelonT m) ModelState
checkSharePriceOld input =
  let
    updatePriceFeed = PriceFeed.updatePriceFeed (input^.miVersion)
    fund = input^.miFund.fdAddress
    priceFeed = input^.miVersion.vdCanonicalPriceFeed
    assets = input^.miVersion.vdAssets.to HashMap.keys

    gen s
      | s^.msIsShutdown = Nothing
      | otherwise = Just $ pure CheckSharePrice
    execute CheckSharePrice = do
      defaultCall <- view ctxCall

      evalM $ updatePriceFeed undefined

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
    [ Require $ \s _ -> not $ s^.msIsShutdown
    , Ensure $ \_prior _after CheckSharePrice
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
  => ModelInput
  -> Command n (MelonT m) ModelState
checkFeeAllocation input =
  let
    updatePriceFeed = PriceFeed.updatePriceFeed (input^.miVersion)
    fund = input^.miFund.fdAddress

    gen s
      | s^.msIsShutdown = Nothing
      | otherwise = Just $ pure CheckFeeAllocation
    execute CheckFeeAllocation = do
      defaultCall <- view ctxCall

      evalM $ updatePriceFeed undefined

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
    [ Require $ \s _ -> not $ s^.msIsShutdown
    , Ensure $ \_prior _after CheckFeeAllocation
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
  => ModelInput
  -> Command n (MelonT m) ModelState
checkMakeOrderSharePrice input =
  let
    fund = input^.miFund.fdAddress
    manager = input^.miFund.fdManager
    updatePriceFeed = PriceFeed.updatePriceFeed (input^.miVersion)
    -- XXX: Could we pick these at random?
    giveAsset = input^.miVersion.vdMlnToken
    getAsset = input^.miVersion.vdEthToken

    gen s
      | s^.msIsShutdown = Nothing
      | otherwise = Just $ CheckMakeOrderSharePrice
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
      evalM $ updatePriceFeed undefined
      priceBeforeOrder <- evalM $ liftWeb3 $ Fund.calcSharePrice callFund

      let managerCallFund = callFund { callFrom = Just manager }
          -- XXX: MatchingMarketAdapter and SimpleAdapter have mismatching signatures.
          --   Is that intentioal? Does that mean one of them will not be found?
          makeOrderSignature = encodeSignature (Proxy @MatchingMarketAdapter.MakeOrderData)
          nullAddress = "0x0000000000000000000000000000000000000000"
      evalM $ liftWeb3
        (Fund.callOnExchange managerCallFund
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
    [ Require $ \s _ -> not $ s^.msIsShutdown
    , Ensure $ \_prior _after CheckMakeOrderSharePrice {}
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
      !(UIntN 256)  -- ^ Exchange index
      !(UIntN 256)  -- ^ Sell quantity
      !(UIntN 256)  -- ^ Get quantity
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
  => ModelInput
  -> Command n (MelonT m) ModelState
checkRequestInvestment input =
  let
    updatePriceFeed = PriceFeed.updatePriceFeed (input^.miVersion)
    fund = input^.miFund.fdAddress
    owner = input^.miVersion.vdOwner

    gen s
      | s^.msIsShutdown = Nothing
      | otherwise = Just $ CheckRequestInvestment
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
      evalM $ updatePriceFeed undefined

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
    [ Require $ \s _ -> not $ s^.msIsShutdown
    , Ensure $ \_prior _after CheckRequestInvestment {}
      (priceBefore, priceAfter) -> do
        -- Property holds exactly.
        priceBefore === priceAfter
    ]

data CheckRequestInvestment (v :: * -> *)
  = CheckRequestInvestment
      !Address -- ^ Investor
      !Address -- ^ Trade token
      !(UIntN 256) -- ^ Give quantity
      !(UIntN 256) -- ^ Share quantity
  deriving (Eq, Show)
instance HTraversable CheckRequestInvestment where
  htraverse _ (CheckRequestInvestment investor token give share)
    = CheckRequestInvestment
    <$> pure investor
    <*> pure token
    <*> pure give
    <*> pure share

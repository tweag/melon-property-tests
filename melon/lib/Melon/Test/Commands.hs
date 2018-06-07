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
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.Ethereum.ABI.Prim.Address (Address)
import Network.Ethereum.ABI.Prim.Int (UIntN)
import Network.Ethereum.Web3.Types (Call (..))

import qualified Melon.ABI.Assets.Asset as Asset
import qualified Melon.ABI.Exchange.Adapter.MatchingMarketAdapter as MatchingMarketAdapter
import qualified Melon.ABI.Fund as Fund
import qualified Melon.ABI.PriceFeeds.CanonicalPriceFeed as CanonicalPriceFeed
import qualified Melon.ABI.RiskMgmt.RMMakeOrders as RMMakeOrders
import qualified Melon.ABI.Version.Version as Version
import Melon.ThirdParty.Network.Ethereum.ABI.Codec (encodeCall, encodeSignature)
import Melon.ThirdParty.Network.Ethereum.Web3.Eth (getTransactionEvents)
import Melon.Context
import qualified Melon.Contract.PriceFeed as PriceFeed
import Melon.Model.Input
import Melon.Model.State


-- | @truncateTo requestedPrecision givenDecimals givenValue@.
truncateTo :: (Integral d, Integral v) => d -> d -> v -> v
truncateTo precision decimals value = value `div` (10^(decimals - precision))


----------------------------------------------------------------------
-- Commands and Invariants on Modelled Fund
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
-- XXX:
--   This test-case is disabled because it causes timeouts with hs-web3 and
--   parity.
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
-- XXX:
--   This test-case assumes that investment is always allowed.
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
      -- XXX:
      --   @Created@ events are not always fired, even if the shares are
      --   created.
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
        -- FAILURE:
        --   Round-off errors in the more complex share-price calculation
        --   within the contract cause a discrepancy between the model's
        --   share-price
        --
        --     sum (assetBalance * assetPrice) / totalShares
        --
        --   and the contract's share-price. For a strongly randomly
        --   fluctuating price-feed, with price-jumps on the order of 100%,
        --   this can lead to differences between the expected and observed
        --   share-price on the order of 10%.
        truncate' sharePrice === truncate' (s^.msSharePrice)
    ]


----------------------------------------------------------------------
-- Commands and Invariants on plain Fund
----------------------------------------------------------------------

------------------------------------------------------------
-- Share Price

newtype CheckSharePrice (v :: * -> *)
  = CheckSharePrice
    (HashMap Address (UIntN 256)) -- ^ Price-update
  deriving (Eq, Show)
instance HTraversable CheckSharePrice where
  htraverse _ (CheckSharePrice prices) = pure $ CheckSharePrice prices

-- | Tests share-price invariant
--
-- Invariant of Sum of asset balances times their price (according to price
-- feed) a fund holds divided by the amount of total shares in existence of
-- this fund (= totalSupply) is equal to its sharePrice:
-- @
--   SumForAllAssets(assetBalance * assetPrice) / totalShares == sharePrice
-- @
simpleCheckSharePrice
  :: (Monad n, Monad m, MonadCatch m, MonadIO m, MonadTest m, MonadThrow m)
  => ModelInput
  -> Command n (MelonT m) SimpleModelState
simpleCheckSharePrice input =
  let
    updatePriceFeed = PriceFeed.updatePriceFeed (input^.miVersion)
    fund = input^.miFund.fdAddress
    priceFeed = input^.miVersion.vdCanonicalPriceFeed
    assets = input^.miVersion.vdAssets.to HashMap.keys
    quoteDecimals = input^.miVersion.vdQuoteDecimals

    gen s = Just $ pure $ CheckSharePrice (s^.smsPriceFeed.to head)
    execute (CheckSharePrice prices) = do
      defaultCall <- view ctxCall

      evalM $ updatePriceFeed prices

      let callPriceFeed = defaultCall { callTo = Just priceFeed }
      (prices', _timestamps) <- evalM $ liftWeb3 $
        CanonicalPriceFeed.getPrices callPriceFeed assets

      balances <- evalM $ liftWeb3 $ forM assets $ \asset -> do
        let callAsset = defaultCall { callTo = Just asset }
        Asset.balanceOf callAsset fund

      let callFund = defaultCall { callTo = Just fund }
      totalShares <- evalM $ liftWeb3 $ Fund.totalSupply callFund
      sharePrice <- evalM $ liftWeb3 $ Fund.calcSharePrice callFund

      pure (prices', balances, totalShares, sharePrice)
  in
  Command gen execute
    [ Update $ \s _ _ -> s & smsPriceFeed %~ tail
    , Ensure $ \_prior _after CheckSharePrice {}
      (prices, balances, totalShares, sharePrice) -> do
        footnote $ "prices " ++ show prices
        footnote $ "balances " ++ show balances
        footnote $ "totalShares " ++ show totalShares
        length prices === length balances
        unless (totalShares == 0) $ do
          let calculatedSharePrice =
                sum (zipWith (*) balances prices)
                `div`
                totalShares
              -- Property only holds to a certain precision.
              truncate' = truncateTo 8 quoteDecimals
          truncate' sharePrice === truncate' calculatedSharePrice
    ]


------------------------------------------------------------
-- Fee Allocation

data CheckFeeAllocation (v :: * -> *) = CheckFeeAllocation
  deriving (Eq, Show)
instance HTraversable CheckFeeAllocation where
  htraverse _ CheckFeeAllocation = pure CheckFeeAllocation

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
  -> Command n (MelonT m) SimpleModelState
checkFeeAllocation input =
  let
    fund = input^.miFund.fdAddress
    quoteDecimals = input^.miVersion.vdQuoteDecimals

    gen _ = Just $ pure CheckFeeAllocation
    execute CheckFeeAllocation = do
      defaultCall <- view ctxCall

      let callFund = defaultCall { callTo = Just fund }
      priceBeforeAlloc <- evalM $ liftWeb3 $ Fund.calcSharePrice callFund
      _ <- evalM $ liftWeb3 $
        Fund.calcSharePriceAndAllocateFees callFund
        >>= getTransactionEvents >>= \case
          [Fund.CalculationUpdate _ _ _ _ sharePrice _] -> pure sharePrice
          _ -> fail "calcSharePriceAndAllocateFees failed"
      priceAfterAlloc <- evalM $ liftWeb3 $ Fund.calcSharePrice callFund

      pure (priceBeforeAlloc, priceAfterAlloc)
  in
  Command gen execute
    [ Ensure $ \_prior _after CheckFeeAllocation
      (priceBeforeAlloc, priceAfterAlloc) -> do
        let truncate' = truncateTo 8 quoteDecimals
        -- Property only holds to a certain precision.
        truncate' priceBeforeAlloc === truncate' priceAfterAlloc
    ]


------------------------------------------------------------
-- Make Order

data CheckMakeOrder (v :: * -> *)
  = CheckMakeOrder
      !(UIntN 256)  -- ^ Exchange index
      !(UIntN 256)  -- ^ Sell quantity
      !(UIntN 256)  -- ^ Get quantity
      !(HashMap Address (UIntN 256)) -- ^ Price-update
  deriving (Eq, Show)
instance HTraversable CheckMakeOrder where
  htraverse _ (CheckMakeOrder exchangeIndex sellQuantity getQuantity prices)
    = CheckMakeOrder
    <$> pure exchangeIndex
    <*> pure sellQuantity
    <*> pure getQuantity
    <*> pure prices

-- | Test make-order share-price property
--
-- Sending of assets to an exchange using a makeOrder does not alter the
-- sharePrice.
checkMakeOrder
  :: ( Monad n, MonadGen n, Monad m
     , MonadCatch m, MonadIO m, MonadTest m, MonadThrow m
     )
  => ModelInput
  -> Command n (MelonT m) SimpleModelState
checkMakeOrder input =
  let
    nullAddress = "0x0000000000000000000000000000000000000000"
    quoteDecimals = input^.miVersion.vdQuoteDecimals
    updatePriceFeed = PriceFeed.updatePriceFeed (input^.miVersion)
    fund = input^.miFund.fdAddress
    manager = input^.miFund.fdManager
    -- XXX: Pick these at random
    giveAsset = input^.miVersion.vdMlnToken
    getAsset = input^.miVersion.vdEthToken

    gen s = Just $ CheckMakeOrder
      -- XXX: MatchingMarketAdapter and SimpleAdapter have mismatching signatures.
      --   Indeed this causes the call to SimpleAdapter (index 0) to fail.
      -- XXX: Number of exchanges is hard-coded
      <$> Gen.integral (Range.linear 1 2)
      <*> Gen.integral (Range.linear 1 (10^(23::Int)))
      <*> Gen.integral (Range.linear 1 (10^(23::Int)))
      <*> pure (s^.smsPriceFeed.to head)
    execute (CheckMakeOrder exchangeIndex sellQuantity getQuantity prices) = do
      defaultCall <- getCall

      --------------------------------------------------
      -- Price feed must be up-to-date
      evalM $ updatePriceFeed prices

      --------------------------------------------------
      -- Check with risk management.
      let callPriceFeed = defaultCall { callTo = Just (input^.miVersion.vdCanonicalPriceFeed) }
      orderPrice <- evalM $ liftWeb3 $
        CanonicalPriceFeed.getOrderPriceInfo callPriceFeed
          giveAsset getAsset sellQuantity getQuantity
      (_, referencePrice, _) <- evalM $ liftWeb3 $
        CanonicalPriceFeed.getReferencePriceInfo callPriceFeed
          giveAsset getAsset

      let callRM = defaultCall { callTo = Just (input^.miVersion.vdRiskManagement) }
      isPermitted <- evalM $ liftWeb3 $
        RMMakeOrders.isMakePermitted callRM
          orderPrice referencePrice giveAsset getAsset sellQuantity getQuantity

      --------------------------------------------------
      -- Share-price before
      let callFund = defaultCall { callTo = Just fund }
      -- FAILURE:
      --   The call to @calcSharePrice@ can fail if the price-feed produces
      --   large asset-prices that lead to overflows in the price calculation
      --   within the fund contract. A number of operations on the fund use
      --   @calcSharePrice@ or similar internally. Such very large asset prices
      --   could render the fund partially unusable.
      priceBeforeOrder <- evalM $ liftWeb3 $ Fund.calcSharePrice callFund

      --------------------------------------------------
      -- Make the order
      let managerCallFund = callFund { callFrom = Just manager }
          -- XXX: MatchingMarketAdapter and SimpleAdapter have mismatching signatures.
          --   Simple market is considered deprecated and should be removed.
          makeOrderSignature = encodeSignature (Proxy @MatchingMarketAdapter.MakeOrderData)
      didSucceed <- evalM $ liftWeb3
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
          [MatchingMarketAdapter.OrderUpdated _exchange _orderId 0] -> do
            annotate "makeOrder succeeded"
            pure True
          _ -> do
            -- Make order might fail for all sorts of reasons.
            -- This test-case is not testing that aspect.
            annotate "makeOrder failed"
            pure False

      --------------------------------------------------
      -- Share-price after
      priceAfterOrder <- evalM $ liftWeb3 $ Fund.calcSharePrice callFund

      pure (priceBeforeOrder, priceAfterOrder, isPermitted, didSucceed)
  in
  Command gen execute
    [ Update $ \s _ _ -> s & smsPriceFeed %~ tail
    , Ensure $ \_prior _after CheckMakeOrder {}
      (priceBeforeOrder, priceAfterOrder, isPermitted, didSucceed) -> do
        -- Share-price should not change immediately.
        let truncate' = truncateTo 8 quoteDecimals
        truncate' priceBeforeOrder === truncate' priceAfterOrder
        -- Order should only be placed if risk management allows it.
        when didSucceed $
          assert isPermitted
    ]


------------------------------------------------------------
-- Request Investment

data CheckRequestInvestment (v :: * -> *)
  = CheckRequestInvestment
      !Address -- ^ Investor
      !Address -- ^ Trade token
      !(UIntN 256) -- ^ Give quantity
      !(UIntN 256) -- ^ Share quantity
      !(HashMap Address (UIntN 256)) -- ^ Price-update 1
      !(HashMap Address (UIntN 256)) -- ^ Price-update 2
  deriving (Eq, Show)
instance HTraversable CheckRequestInvestment where
  htraverse _ (CheckRequestInvestment investor token give share prices prices')
    = CheckRequestInvestment
    <$> pure investor
    <*> pure token
    <*> pure give
    <*> pure share
    <*> pure prices
    <*> pure prices'

-- | Test request-investment property
--
-- - Investment and redemption by regular means (request and execute, not
--     @emergencyRedeem@) do not immediately alter share price
checkRequestInvestment
  :: ( Monad n, MonadGen n, Monad m
     , MonadCatch m, MonadIO m, MonadTest m, MonadThrow m
     )
  => ModelInput
  -> Command n (MelonT m) SimpleModelState
checkRequestInvestment input =
  let
    updatePriceFeed = PriceFeed.updatePriceFeed (input^.miVersion)
    fund = input^.miFund.fdAddress
    owner = input^.miVersion.vdOwner

    gen s = Just $ CheckRequestInvestment
          <$> Gen.element (input^.miInvestors)
          <*> Gen.element (input^.miVersion.vdAssets.to HashMap.keys)
          <*> Gen.integral (Range.linear 1 100000)
          <*> Gen.integral (Range.linear 1 100000)
          <*> pure (s^.smsPriceFeed.to head)
          <*> pure (s^.smsPriceFeed.to (!!2))
    execute (CheckRequestInvestment investor token give share prices prices') = do
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

      -- Price-feed must be updated
      annotateShow prices
      evalM $ updatePriceFeed prices

      -- Total shares before
      sharesBefore <- evalM $ liftWeb3 $
        Fund.totalSupply callFund

      -- Share-price before request
      priceBeforeRequest <- evalM $ liftWeb3 $
        Fund.calcSharePrice callFund

      -- Request investment
      reqId <- evalM $ liftWeb3 $
        Fund.requestInvestment investorCallFund give share token
        >>= getTransactionEvents >>= \case
          [Fund.RequestUpdated reqId] -> pure reqId
          _ -> fail "Investment request failed."

      -- Share-price after request
      priceAfterRequest <- evalM $ liftWeb3 $
        Fund.calcSharePrice callFund

      -- Transfer the amount
      evalM $ liftWeb3 $
        Asset.transfer ownerCallToken investor give
        >>= getTransactionEvents >>= \case
          [Asset.Transfer {}] -> pure ()
          _ -> fail "Token transfer failed."

      -- Approve the amount
      evalM $ liftWeb3 $
        Asset.approve investorCallToken fund give
        >>= getTransactionEvents >>= \case
          [Asset.Approval {}] -> pure ()
          _ -> fail "Investor token approval failed."

      -- Price-feed must be updated twice
      evalM $ updatePriceFeed prices
      evalM $ updatePriceFeed prices

      -- Share-price before execute
      priceBeforeExecute <- evalM $ liftWeb3 $
        Fund.calcSharePrice callFund

      -- Attempt to execute request.
      void $ evalM $ liftWeb3 $
        Fund.executeRequest investorCallFund reqId

      -- Share-price after execute
      priceAfterExecute <- evalM $ liftWeb3 $
        Fund.calcSharePrice callFund

      -- Price-feed must be updated
      evalM $ updatePriceFeed prices'

      pure
        ( sharesBefore
        , priceBeforeRequest, priceAfterRequest
        , priceBeforeExecute, priceAfterExecute
        )
  in
  Command gen execute
    [ Update $ \s _ _ -> s & smsPriceFeed %~ (drop 2)
    , Ensure $ \_prior _after CheckRequestInvestment {}
      ( _sharesBefore
      , priceBeforeRequest, priceAfterRequest
      , _priceBeforeExecute, _priceAfterExecute
      ) -> do
        priceBeforeRequest === priceAfterRequest
        -- FAILURE:
        --   This property fails. E.g.
        --
        --     priceBeforeExecute === priceAfterExecute
        --     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        --     │ Failed (- lhs =/= + rhs)
        --     │ - 988274706867671691
        --     │ + 987886944818304172
        --
        -- unless (sharesBefore == 0) $
        --   priceBeforeExecute === priceAfterExecute
    ]

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Melon.Deploy where

import Control.Lens ((^.))
import Control.Monad (forM_, replicateM, unless, void)
import Data.Proxy
import Data.Text (Text)
import Network.Ethereum.ABI.Prim.Address (Address)
import Network.Ethereum.ABI.Prim.Int (UIntN)
import Network.Ethereum.Web3.Eth
import Network.Ethereum.Web3.Provider
import Network.Ethereum.Web3.Types
import System.IO

import qualified Melon.ABI.Assets.PreminedAsset as PreminedAsset
import qualified Melon.ABI.Compliance.NoCompliance as NoCompliance
import qualified Melon.ABI.Exchange.ThirdParty.MatchingMarket as MatchingMarket
import qualified Melon.ABI.Exchange.ThirdParty.SimpleMarket as SimpleMarket
-- import qualified Melon.ABI.Exchange.Adapter.CentralizedAdapter as CentralizedAdapter
import qualified Melon.ABI.Exchange.Adapter.MatchingMarketAdapter as MatchingMarketAdapter
import qualified Melon.ABI.Exchange.Adapter.SimpleAdapter as SimpleAdapter
import qualified Melon.ABI.Fund as Fund
-- import qualified Melon.ABI.FundRanking as FundRanking
import qualified Melon.ABI.PriceFeeds.CanonicalPriceFeed as CanonicalPriceFeed
import qualified Melon.ABI.RiskMgmt.RMMakeOrders as RMMakeOrders
import qualified Melon.ABI.System.Governance as Governance
import qualified Melon.ABI.Version.Version as Version
import Melon.Context
import qualified Melon.Contract.Governance as Governance
import qualified Melon.Contract.PreminedAsset as PreminedAsset
import Melon.Contract.PriceFeed (updateCanonicalPriceFeed)
import qualified Melon.Contract.PriceFeed as PriceFeed
import Melon.Contract.TermsAndConditions
import Melon.ThirdParty.Network.Ethereum.ABI.Codec
import Melon.ThirdParty.Network.Ethereum.Web3.Eth


mockBytes :: Text
mockBytes = "0x86b5eed81db5f691c36cc83eb58cb5205bd2090bf3763a19f0c5bf2f074dd84b"


mockAddress :: Address
mockAddress = "0x083c41ea13af6c2d5aaddf6e73142eb9a7b00183"


deploy :: IO
  ( Address -- ^ Melon fund
  , Address -- ^ Price feed
  , MelonT Web3 () -- ^ Update the price feed
  , UIntN 256 -- ^ Number of registered exchanges
  , Address -- ^ Fund manager
  , [Address] -- ^ Registered assets
  )
deploy = do
  hSetBuffering stdout LineBuffering
  r <- runWeb3 $ runMelonT $ do

    ------------------------------------------------------------
    -- Accounts
    owner:manager:investorA:_ <- liftWeb3 accounts

    defaultCall <- withContext $ \ctx -> pure (ctx^.ctxCall)
    let ownerCall = defaultCall { callFrom = Just owner }
        managerCall = defaultCall { callFrom = Just manager }
        investorACall = defaultCall { callFrom = Just investorA }

    ------------------------------------------------------------
    -- Assets
    [ethToken, mlnToken, eurToken] <- replicateM 3 $
      PreminedAsset.deploy owner
    let _callEth = defaultCall { callTo = Just ethToken }
        _callMln = defaultCall { callTo = Just mlnToken }
        _callEur = defaultCall { callTo = Just eurToken }
        _ownerCallEth = ownerCall { callTo = Just ethToken }
        ownerCallMln = ownerCall { callTo = Just mlnToken }
        _ownerCallEur = ownerCall { callTo = Just eurToken }

    ------------------------------------------------------------
    -- Governance
    governance <- Governance.deploy owner

    ------------------------------------------------------------
    -- Price feed
    (canonicalPriceFeed, stakingPriceFeed) <- PriceFeed.deploy owner mlnToken governance

    ------------------------------------------------------------
    -- Markets
    -- XXX: The simple market adapter has a different make order interface than
    --   the matching market adapter. Should we really combine those?
    simpleMarket <- liftWeb3 $
      getContractAddress =<<
      SimpleMarket.constructor ownerCall
    simpleAdapter <- liftWeb3 $
      getContractAddress =<<
      SimpleAdapter.constructor ownerCall
    matchingMarket <- liftWeb3 $
      getContractAddress =<<
      MatchingMarket.constructor ownerCall
        154630446100 -- close time
    matchingMarketAdapter <- liftWeb3 $
      getContractAddress =<<
      MatchingMarketAdapter.constructor ownerCall

    ------------------------------------------------------------
    -- Compliance
    noCompliance <- liftWeb3 $
      getContractAddress =<<
      NoCompliance.constructor ownerCall

    ------------------------------------------------------------
    -- Risk Management
    rmMakeOrders <- liftWeb3 $
      getContractAddress =<<
      RMMakeOrders.constructor ownerCall

    ------------------------------------------------------------
    -- Centralized exchange adpater
    -- XXX: Not currently used.
    -- centralizedAdapter <- liftWeb3 $
    --   getContractAddress =<<
    --   CentralizedAdapter.constructor ownerCall

    ------------------------------------------------------------
    -- Version
    version <- liftWeb3 $
      getContractAddress =<<
      Version.constructor ownerCall
        "0.7.2-alpha.1" -- melon protocol version
        governance -- governance contract
        ethToken -- native asset address
        canonicalPriceFeed -- canonical pricefeed address
        False -- is-mainnet?

    ------------------------------------------------------------
    -- Fund ranking
    -- XXX: Not currently used.
    -- fundRanking <- liftWeb3 $
    --   getContractAddress =<<
    --   FundRanking.constructor ownerCall

    ------------------------------------------------------------
    -- White list trading pairs
    -- XXX: How important is the order here?
    --   This could be combined with the market creation above.
    let tradingPairs = [(mlnToken, ethToken), (eurToken, ethToken), (mlnToken, eurToken)]
        ownerCallMatchingMarket = ownerCall { callTo = Just matchingMarket }
    liftWeb3 $ forM_ tradingPairs $ \(base, quote) ->
      MatchingMarket.addTokenPairWhitelist ownerCallMatchingMarket
        base quote
        >>= getTransactionEvents >>= \case
        [MatchingMarket.LogAddTokenPairWhitelist base' quote'] ->
          unless (base == base' && quote == quote') $
            error "Failed to add token pair to whitelist"
        _ -> error "Failed to add token pair to whitelist"

    ------------------------------------------------------------
    -- Add Version to Governance tracking
    -- XXX: How important is the order here?
    --   This could be combined with the version creation above.
    let registerVersion = encodeCall $ Governance.AddVersionData version
    Governance.action governance owner governance registerVersion 0

    ------------------------------------------------------------
    -- Register markets with price feed
    -- XXX: How important is the order here?
    --   This could be combined with the market creation above,
    --   or even with the price feed creation.
    -- XXX: @utils/deploy/contracts.js@ registers MatchingMarket twice.
    let numExchanges = 2
    let registerSimpleMarket = encodeCall $ CanonicalPriceFeed.RegisterExchangeData
          simpleMarket -- exchange address
          simpleAdapter -- exchange adapter address
          True -- whether exchange takes custody of tokens before trading
          [ encodeSignature (Proxy @SimpleAdapter.MakeOrderData)
          , encodeSignature (Proxy @SimpleAdapter.TakeOrderData)
          , encodeSignature (Proxy @SimpleAdapter.CancelOrderData)
          ] -- function signatures
    Governance.action governance owner canonicalPriceFeed registerSimpleMarket 0
    let registerMatchingMarket = encodeCall $ CanonicalPriceFeed.RegisterExchangeData
          matchingMarket -- exchange address
          matchingMarketAdapter -- exchange adapter address
          True -- whether exchange takes custody of tokens before trading
          [ encodeSignature (Proxy @MatchingMarketAdapter.MakeOrderData)
          , encodeSignature (Proxy @MatchingMarketAdapter.TakeOrderData)
          , encodeSignature (Proxy @MatchingMarketAdapter.CancelOrderData)
          ] -- function signatures
    Governance.action governance owner canonicalPriceFeed registerMatchingMarket 0

    ------------------------------------------------------------
    -- Register tokens with price feed
    -- XXX: How important is the order here?
    --   This could be combined with the price feed creation above.
    let registerEtherToken = encodeCall $ CanonicalPriceFeed.RegisterAssetData
          ethToken -- asset address
          "Ether token" -- asset name
          "ETH-T" -- asset symbol
          18 -- asset decimal places
          "ethereum.org" -- asset related URL
          mockBytes -- asset IPFS hash
          mockAddress -- asset break-in address
          mockAddress -- asset break-out address
          [] -- asset EIP standards
          [] -- asset white listed functions
    Governance.action governance owner canonicalPriceFeed registerEtherToken 0
    let registerEuroToken = encodeCall $ CanonicalPriceFeed.RegisterAssetData
          eurToken -- asset address
          "Euro token" -- asset name
          "EUR-T" -- asset symbol
          -- XXX: Should this be 8 or 18?
          --   The EUR token is specified to have 8 decimals. But, the
          --   javascript code sets this value to 18 nonetheless.
          18 -- asset decimal places
          "europa.eu" -- asset related URL
          mockBytes -- asset IPFS hash
          mockAddress -- asset break-in address
          mockAddress -- asset break-out address
          [] -- asset EIP standards
          [] -- asset white listed functions
    Governance.action governance owner canonicalPriceFeed registerEuroToken 0

    ------------------------------------------------------------
    -- Verify version deployment
    let callVersion = defaultCall { callTo = Just version }
    governance' <- liftWeb3 $ Version.gOVERNANCE callVersion
    unless (governance' == governance) $
      error "Governance mismatch"
    let callGovernance = defaultCall { callTo = Just governance }
    numVersions <- liftWeb3 $ Governance.getVersionsLength callGovernance
    (version', _, _) <- liftWeb3 $ Governance.versions callGovernance (pred numVersions)
    unless (version' == version) $
      error "Version mismatch"
    ethToken' <- liftWeb3 $ Version.getNativeAsset callVersion
    unless (ethToken' == ethToken) $
      error "Native Token mismatch"

    ------------------------------------------------------------
    -- Version deployment complete
    -- XXX: Separate out the following fund setup.

    let managerCallVersion = managerCall { callTo = Just version }

    ------------------------------------------------------------
    -- Setup a fund instance
    (r, s, v) <- liftWeb3 $ getTermsSignatureParameters manager
    fund <- liftWeb3 $ Version.setupFund managerCallVersion
      "Test fund" -- fund name
      mlnToken -- quote asset
      (10^(16::Int)) -- management fee
      10 -- performance fee
      noCompliance -- participation module address
      rmMakeOrders -- risk management module address
      [simpleMarket, matchingMarket] -- addresses of exchanges where the fund can trade
      v r s -- signature elliptic curve parameters
      >>= getTransactionEvents >>= \case
      [Version.FundUpdated fund] -> pure fund
      _ -> error "Expected one FundUpdated event"
    fund' <- liftWeb3 $ Version.managerToFunds callVersion manager
    unless (fund' == fund) $
      error "Fund address mismatch"

    let managerCallFund = managerCall { callTo = Just fund }
        callFund = defaultCall { callTo = Just fund }

    ------------------------------------------------------------
    -- Enable investment and redemption
    -- XXX: Move this into tests and make it part of the model.
    void $ liftWeb3 $ Fund.enableInvestment managerCallFund
      [ethToken, eurToken]
    void $ liftWeb3 $ Fund.enableRedemption managerCallFund
      [ethToken, eurToken]
    let investTokens =
          [ ("MLN-T", mlnToken)
          , ("ETH-T", ethToken)
          , ("EUR-T", eurToken)
          ]

    ------------------------------------------------------------
    -- Verify enabled investment and redemption
    -- XXX: Move this into tests and make it part of the model.
    liftWeb3 $ forM_ investTokens $ \(sym, token) -> do
      investAllowed <- Fund.isInvestAllowed callFund token
      unless investAllowed $ error $ "Investment not allowed for " ++ sym
      redeemAllowed <- Fund.isRedeemAllowed callFund token
      unless redeemAllowed $ error $ "Redemption not allowed for " ++ sym

    ------------------------------------------------------------
    -- Initial price feed update
    -- XXX: Should this just happen every 30 seconds or so?
    -- XXX: Replace by mock price feed update.
    let updatePriceFeed =
          updateCanonicalPriceFeed
            canonicalPriceFeed
            stakingPriceFeed
            owner
            "MLN"
            18
            [ ("MLN", mlnToken, 18)
            , ("ETH", ethToken, 18)
            , ("EUR", eurToken,  8)
            ]
    updatePriceFeed

    ------------------------------------------------------------
    -- Initial calculate share price
    liftWeb3 $
      Fund.calcGav callFund
      >>= getTransactionEvents >>= \case
        [Fund.PortfolioContent {}] -> pure ()
        _ -> error "Expected one PortfolioContent event"
    void $ liftWeb3 $ Fund.calcSharePrice callFund

    ------------------------------------------------------------
    -- Three initial trades
    -- XXX: Move this into tests and the model
    let initialTokenAmount = 10^(23::Int)
    void $ liftWeb3 $ PreminedAsset.transfer ownerCallMln
      investorA initialTokenAmount
      >>= getTransactionEvents >>= \case
        [PreminedAsset.Transfer {}] -> pure ()
        _ -> error "Initial token transfer failed"

    let tradeToken = mlnToken
        investorACallFund = investorACall { callTo = Just fund }
        investorACallMln = investorACall { callTo = Just mlnToken }
        trades = [ (2000, 2000), (10^(18::Int), 10^(18::Int)), ((10^(18::Int)) `div` 2, 10^(18::Int)) ]
    forM_ trades $ \(shareQuantity, giveQuantity) -> do

      -- Approve asset
      liftWeb3 $
        PreminedAsset.approve investorACallMln
          fund giveQuantity
        >>= getTransactionEvents >>= \case
          [PreminedAsset.Approval from to allowance] ->
            unless (from == investorA && to == fund && allowance == giveQuantity) $
              error "investor token allowance failed."
          _ -> error "investor token allowance failed."

      -- Request investment
      reqId <- liftWeb3 $ Fund.requestInvestment investorACallFund
        giveQuantity -- give quantity (MLN-T)
        shareQuantity -- ask quantity (shares)
        tradeToken -- asset to invest in
        >>= getTransactionEvents >>= \case
          [Fund.RequestUpdated reqId] -> pure reqId
          _ -> error "Expected one RequestUpdated event"

      -- Update price-feed twice
      updatePriceFeed >> updatePriceFeed

      -- Execute investment
      void $ liftWeb3 $ Fund.executeRequest investorACallFund reqId
    updatePriceFeed
    liftWeb3 $
      Fund.calcGav callFund
      >>= getTransactionEvents >>= \case
        [Fund.PortfolioContent {}] -> pure ()
        _ -> error "Expected one PortfolioContent event"
    void $ liftWeb3 $ Fund.calcSharePrice callFund

    updatePriceFeed

    return (fund, canonicalPriceFeed, updatePriceFeed, numExchanges, manager, [mlnToken, ethToken, eurToken])

  case r of
    Left err -> error $ "deploy failed: " ++ show err
    Right x -> return x

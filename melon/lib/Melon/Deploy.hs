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
import Control.Monad.IO.Class
import Data.Proxy
import Data.Text (Text)
import Network.Ethereum.ABI.Prim.Address (Address)
import Network.Ethereum.ABI.Prim.Int (UIntN)
import Network.Ethereum.Web3.Eth
import Network.Ethereum.Web3.Provider
import Network.Ethereum.Web3.Types
import System.IO

import qualified Melon.ABI.Assets.Asset as Asset
import qualified Melon.ABI.Assets.PreminedAsset as PreminedAsset
import qualified Melon.ABI.Compliance.NoCompliance as NoCompliance
import qualified Melon.ABI.Exchange.ThirdParty.MatchingMarket as MatchingMarket
import qualified Melon.ABI.Exchange.ThirdParty.SimpleMarket as SimpleMarket
import qualified Melon.ABI.Exchange.Adapter.CentralizedAdapter as CentralizedAdapter
import qualified Melon.ABI.Exchange.Adapter.MatchingMarketAdapter as MatchingMarketAdapter
import qualified Melon.ABI.Exchange.Adapter.SimpleAdapter as SimpleAdapter
import qualified Melon.ABI.Fund as Fund
import qualified Melon.ABI.FundRanking as FundRanking
import qualified Melon.ABI.PriceFeeds.CanonicalPriceFeed as CanonicalPriceFeed
import qualified Melon.ABI.PriceFeeds.StakingPriceFeed as StakingPriceFeed
import qualified Melon.ABI.RiskMgmt.RMMakeOrders as RMMakeOrders
import qualified Melon.ABI.System.Governance as Governance
import qualified Melon.ABI.Version.Version as Version
import Melon.Context
import Melon.Contract.Governance
import Melon.Contract.PriceFeed
import Melon.Contract.TermsAndConditions
import Melon.ThirdParty.Network.Ethereum.ABI.Codec
import Melon.ThirdParty.Network.Ethereum.Web3.Eth


mockBytes :: Text
mockBytes = "0x86b5eed81db5f691c36cc83eb58cb5205bd2090bf3763a19f0c5bf2f074dd84b"


mockAddress :: Address
mockAddress = "0x083c41ea13af6c2d5aaddf6e73142eb9a7b00183"


deploy :: IO (Address, Address, [Address])
deploy = do
  hSetBuffering stdout LineBuffering
  r <- runWeb3 $ runMelonT $ do
    owner:manager:investorA:_ <- liftWeb3 accounts

    defaultCall <- withContext $ \ctx -> pure (ctx^.ctxCall)
    let ownerCall = defaultCall { callFrom = Just owner }
        managerCall = defaultCall { callFrom = Just manager }
        investorACall = defaultCall { callFrom = Just investorA }

    liftIO $ putStrLn "Tokens"
    [ethToken, mlnToken, eurToken] <- liftWeb3 $ replicateM 3 $
      getContractAddress =<<
      PreminedAsset.constructor ownerCall
    let _callEth = defaultCall { callTo = Just ethToken }
        callMln = defaultCall { callTo = Just mlnToken }
        _callEur = defaultCall { callTo = Just eurToken }
        _ownerCallEth = ownerCall { callTo = Just ethToken }
        ownerCallMln = ownerCall { callTo = Just mlnToken }
        _ownerCallEur = ownerCall { callTo = Just eurToken }

    liftIO $ putStrLn "Governance"
    governance <- liftWeb3 $
      getContractAddress =<<
      Governance.constructor ownerCall
        [owner] -- authorities
        1 -- quorum
        100000 -- window

    liftIO $ putStrLn "CanonicalPriceFeed"
    canonicalPriceFeed <- liftWeb3 $
      getContractAddress =<<
      CanonicalPriceFeed.constructor ownerCall
        mlnToken -- quote asset
        "Melon Token" -- quote asset name
        "MLN-T" -- quote asset symbol
        18 -- quote asset decimal places
        "" --"melonport.com" -- quote asset related URL
        mockBytes -- quote asset IPFS hash
        mockAddress -- quote asset break-in address
        mockAddress -- quote asset break-out address
        [] -- quote asset EIP standards
        [] -- quote asset white listed functions
        [0, 60] -- update-info: interval, validity
        [1000000, 4] -- staking-info: minStake, numOperators
        governance -- address of Governance

    liftIO $ putStrLn "StakingPriceFeed"
    stakingPriceFeed <- liftWeb3 $
      getContractAddress =<<
      StakingPriceFeed.constructor ownerCall
        canonicalPriceFeed -- canonical registrar address
        mlnToken -- quote asset
        canonicalPriceFeed -- superfeed address
    liftIO $ putStrLn "asset.approve"
    void $ liftWeb3 $ Asset.approve ownerCallMln
      stakingPriceFeed -- allowed spender
      1000000 -- approved amount
    liftIO $ putStrLn "stakingPriceFeed.depositStake"
    let ownerCallStaking = ownerCall { callTo = Just stakingPriceFeed }
    void $ liftWeb3 $ StakingPriceFeed.depositStake ownerCallStaking
      1000000 -- amount to stake for this feed
      "" -- dummy, for future use

    liftIO $ putStrLn "SimpleMarket"
    simpleMarket <- liftWeb3 $
      getContractAddress =<<
      SimpleMarket.constructor ownerCall
    liftIO $ putStrLn "SimpleAdapter"
    simpleAdapter <- liftWeb3 $
      getContractAddress =<<
      SimpleAdapter.constructor ownerCall
    liftIO $ putStrLn "MatchingMarket"
    matchingMarket <- liftWeb3 $
      getContractAddress =<<
      MatchingMarket.constructor ownerCall
        154630446100 -- close time
    liftIO $ putStrLn "MatchingMarketAdapter"
    matchingMarketAdapter <- liftWeb3 $
      getContractAddress =<<
      MatchingMarketAdapter.constructor ownerCall

    liftIO $ putStrLn "NoCompliance"
    noCompliance <- liftWeb3 $
      getContractAddress =<<
      NoCompliance.constructor ownerCall
    liftIO $ putStrLn "RMMakeOrders"
    rmMakeOrders <- liftWeb3 $
      getContractAddress =<<
      RMMakeOrders.constructor ownerCall
    liftIO $ putStrLn "CentralizedAdapter"
    _centralizedAdapter <- liftWeb3 $
      getContractAddress =<<
      CentralizedAdapter.constructor ownerCall
    liftIO $ putStrLn "Version"
    version <- liftWeb3 $
      getContractAddress =<<
      Version.constructor ownerCall
        "0.7.2-alpha.1" -- melon protocol version
        governance -- governance contract
        ethToken -- native asset address
        canonicalPriceFeed -- canonical pricefeed address
        False -- is-mainnet?
    liftIO $ putStrLn "FundRanking"
    _fundRanking <- liftWeb3 $
      getContractAddress =<<
      FundRanking.constructor ownerCall

    liftIO $ putStrLn "Whitelist trading pairs"
    let tradingPairs = [(mlnToken, ethToken), (eurToken, ethToken), (mlnToken, eurToken)]
        ownerCallMatchingMarket = ownerCall { callTo = Just matchingMarket }
    liftWeb3 $ forM_ tradingPairs $ \(base, quote) -> do
      liftIO $ putStrLn $ "addTokenPairWhitelist " ++ show base ++ " " ++ show quote
      MatchingMarket.addTokenPairWhitelist ownerCallMatchingMarket
        base quote
        >>= getTransactionEvents >>= \case
        [MatchingMarket.LogAddTokenPairWhitelist base' quote'] ->
          unless (base == base' && quote == quote') $
            error "Failed to add token pair to whitelist"
        _ -> error "Failed to add token pair to whitelist"

    liftIO $ putStrLn "Add Version to Governance tracking"
    let registerVersion = encodeCall $ Governance.AddVersionData version
    governanceAction governance owner governance registerVersion 0

    -- XXX: @utils/deploy/contracts.js@ registers MatchingMarket twice.
    liftIO $ putStrLn "Whitelist SimpleMarket exchange"
    let registerSimpleMarket = encodeCall $ CanonicalPriceFeed.RegisterExchangeData
          simpleMarket -- exchange address
          simpleAdapter -- exchange adapter address
          True -- whether exchange takes custody of tokens before trading
          [ encodeSignature (Proxy @SimpleAdapter.MakeOrderData)
          , encodeSignature (Proxy @SimpleAdapter.TakeOrderData)
          , encodeSignature (Proxy @SimpleAdapter.CancelOrderData)
          ] -- function signatures
    governanceAction governance owner canonicalPriceFeed registerSimpleMarket 0
    liftIO $ putStrLn "Whitelist MatchingMarket exchange"
    let registerMatchingMarket = encodeCall $ CanonicalPriceFeed.RegisterExchangeData
          matchingMarket -- exchange address
          matchingMarketAdapter -- exchange adapter address
          True -- whether exchange takes custody of tokens before trading
          [ encodeSignature (Proxy @MatchingMarketAdapter.MakeOrderData)
          , encodeSignature (Proxy @MatchingMarketAdapter.TakeOrderData)
          , encodeSignature (Proxy @MatchingMarketAdapter.CancelOrderData)
          ] -- function signatures
    governanceAction governance owner canonicalPriceFeed registerMatchingMarket 0

    liftIO $ putStrLn "Register Ether token"
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
    governanceAction governance owner canonicalPriceFeed registerEtherToken 0
    liftIO $ putStrLn "Register Euro token"
    let registerEuroToken = encodeCall $ CanonicalPriceFeed.RegisterAssetData
          eurToken -- asset address
          "Euro token" -- asset name
          "EUR-T" -- asset symbol
          8 -- asset decimal places
          "europa.eu" -- asset related URL
          mockBytes -- asset IPFS hash
          mockAddress -- asset break-in address
          mockAddress -- asset break-out address
          [] -- asset EIP standards
          [] -- asset white listed functions
    governanceAction governance owner canonicalPriceFeed registerEuroToken 0

    liftIO $ putStrLn "Verify"
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

    let managerCallVersion = managerCall { callTo = Just version }

    liftIO $ putStrLn "Setup Fund"
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

    liftIO $ putStrLn "Enable investment and redemption"
    void $ liftWeb3 $ Fund.enableInvestment managerCallFund
      [ethToken, eurToken]
    void $ liftWeb3 $ Fund.enableRedemption managerCallFund
      [ethToken, eurToken]
    let investTokens =
          [ ("MLN-T", mlnToken)
          , ("ETH-T", ethToken)
          , ("EUR-T", eurToken)
          ]
    liftWeb3 $ forM_ investTokens $ \(sym, token) -> do
      investAllowed <- Fund.isInvestAllowed callFund token
      unless investAllowed $ error $ "Investment not allowed for " ++ sym
      redeemAllowed <- Fund.isRedeemAllowed callFund token
      unless redeemAllowed $ error $ "Redemption not allowed for " ++ sym

    liftIO $ putStrLn "Update pricefeed"
    let updatePriceFeed =
          updateCanonicalPriceFeed
            canonicalPriceFeed
            stakingPriceFeed
            owner
            "MLN"
            [ ("MLN", mlnToken, 18)
            , ("ETH", ethToken, 18)
            , ("EUR", eurToken,  8)
            ]
    updatePriceFeed

    liftIO $ putStrLn "Fetch prices"
    let callCanonical = defaultCall { callTo = Just canonicalPriceFeed }
    liftWeb3 $ forM_ investTokens $ \(sym, token) -> do
      (recent, price, digits) <-
        CanonicalPriceFeed.getPriceInfo callCanonical token
      liftIO $ putStrLn $ sym ++ ":"
        ++ " Recent: " ++ show recent
        ++ " Price: " ++ show price
        ++ " Decimals: " ++ show digits
        ++ " ~> " ++ show (fromIntegral price / fromIntegral (10^digits::UIntN 256) :: Double)

    liftIO $ putStrLn "=================================================="
    liftIO $ putStrLn "Initial Share price"
    liftWeb3 $ Fund.calcGav callFund
      >>= getTransactionEvents >>= \case
        [Fund.PortfolioContent assets holdings prices] ->
          liftIO $ print $ Fund.PortfolioContent assets holdings prices
        _ -> error "Expected one PortfolioContent event"
    iniSharePrice <- liftWeb3 $ Fund.calcSharePrice callFund
    liftIO $ putStrLn $ "initial sharePrice: " ++ show iniSharePrice
    supply <- liftWeb3 $ Fund.totalSupply callFund
    liftIO $ putStrLn $ "totalSupply: " ++ show supply

    forM_ [("MLN-T", mlnToken), ("ETH-T", ethToken), ("EUR-T", eurToken)] $ \(sym, token) -> do
      balance <- liftWeb3 $ Fund.balanceOf callFund token
      liftIO $ putStrLn $ sym ++ "(" ++ show token ++ "): " ++ "balance: " ++ show balance
      investAllowed <- liftWeb3 $ Fund.isInvestAllowed callFund token
      liftIO $ putStrLn $ "    invest-allowed: " ++ show investAllowed

    liftIO $ putStrLn "=================================================="
    liftIO $ putStrLn "Fund properties"
    liftWeb3 $ do
      Fund.isShutDown callFund
        >>= \b -> liftIO $ putStrLn $ "isShutDown: " ++ show b
      Fund.isInvestAllowed callFund mlnToken
        >>= \b -> liftIO $ putStrLn $ "isInvestAllowed[MLN-T]: " ++ show b
      Fund.isInvestAllowed callFund ethToken
        >>= \b -> liftIO $ putStrLn $ "isInvestAllowed[ETH-T]: " ++ show b
      Fund.isInvestAllowed callFund eurToken
        >>= \b -> liftIO $ putStrLn $ "isInvestAllowed[EUR-T]: " ++ show b

    liftIO $ putStrLn "=================================================="
    liftIO $ putStrLn "Initial Investment"
    let initialTokenAmount = 10^(23::Int)
    liftIO $ putStrLn $ "Initial transfer"
      ++ " from: " ++ show owner
      ++ " to: " ++ show investorA
      ++ " amount: " ++ show initialTokenAmount
      ++ " token: " ++ show mlnToken
    void $ liftWeb3 $ PreminedAsset.transfer ownerCallMln
      investorA initialTokenAmount
      >>= getTransactionEvents >>= \case
        [t@PreminedAsset.Transfer {}] -> liftIO $ print t
        ts -> liftIO $ putStrLn $ "transfer log: " ++ show ts
    liftWeb3 $
      PreminedAsset.balanceOf callMln investorA >>= \balance ->
      liftIO $ putStrLn $ "MLN-balance: " ++ show balance

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
      liftIO $ putStrLn $ "Request ID: " ++ show reqId
      liftWeb3 $
        Fund.requests callFund reqId
        >>= \(participant, status, type_, asset, shareQuant, giveQuant, receiveQuant, timestamp, updateId) ->
          liftIO $ putStrLn $ "Request status"
            ++ " participant: " ++ show participant
            ++ " status: " ++ show status
            ++ " type_: " ++ show type_
            ++ " asset: " ++ show asset
            ++ " shareQuant: " ++ show shareQuant
            ++ " giveQuant: " ++ show giveQuant
            ++ " receiveQuant: " ++ show receiveQuant
            ++ " timestamp: " ++ show timestamp
            ++ " updateId: " ++ show updateId

      -- Compliance
      let callNoCompliance = defaultCall { callTo = Just noCompliance }
      liftWeb3 $
        NoCompliance.isInvestmentPermitted callNoCompliance
          investorA giveQuantity shareQuantity
        >>= \b -> liftIO $ putStrLn $ "Investment permitted: " ++ show b

      -- Pre-trade stats
      liftWeb3 $ do
        sharePrice <- Fund.calcSharePrice callFund
        requestedSharesValue <- Fund.toWholeShareUnit callFund (sharePrice * shareQuantity)
        let offerRemainder = giveQuantity - requestedSharesValue
        investorPreShares <- Fund.balanceOf callFund investorA
        liftIO $ putStrLn $
          "Pre-trade\n\
          \  share-price: " ++ show sharePrice ++ "\n\
          \  requested-value: " ++ show requestedSharesValue ++ "\n\
          \  remainder: " ++ show offerRemainder ++ "\n\
          \  investor-balance: " ++ show investorPreShares

      -- Update price-feed twice
      updatePriceFeed >> updatePriceFeed

      -- Execute investment
      void $ liftWeb3 $ Fund.executeRequest investorACallFund reqId

      -- Pre-trade stats
      liftWeb3 $ do
        sharePrice <- Fund.calcSharePrice callFund
        requestedSharesValue <- Fund.toWholeShareUnit callFund (sharePrice * shareQuantity)
        let offerRemainder = giveQuantity - requestedSharesValue
        investorPreShares <- Fund.balanceOf callFund investorA
        liftIO $ putStrLn $
          "Post-trade\n\
          \  share-price: " ++ show sharePrice ++ "\n\
          \  requested-value: " ++ show requestedSharesValue ++ "\n\
          \  remainder: " ++ show offerRemainder ++ "\n\
          \  investor-balance: " ++ show investorPreShares

      liftWeb3 $
        Fund.requests callFund reqId
        >>= \(participant, status, type_, asset, shareQuant, giveQuant, receiveQuant, timestamp, updateId) ->
          liftIO $ putStrLn $ "Request status"
            ++ " participant: " ++ show participant
            ++ " status: " ++ show status
            ++ " type_: " ++ show type_
            ++ " asset: " ++ show asset
            ++ " shareQuant: " ++ show shareQuant
            ++ " giveQuant: " ++ show giveQuant
            ++ " receiveQuant: " ++ show receiveQuant
            ++ " timestamp: " ++ show timestamp
            ++ " updateId: " ++ show updateId

    liftIO $ putStrLn "=================================================="
    liftIO $ putStrLn "New Share price"
    updatePriceFeed
    liftWeb3 $
      Fund.calcGav callFund
      >>= getTransactionEvents >>= \case
        [Fund.PortfolioContent assets holdings prices] ->
          liftIO $ print $ Fund.PortfolioContent assets holdings prices
        _ -> error "Expected one PortfolioContent event"
    sharePrice' <- liftWeb3 $ Fund.calcSharePrice callFund
    liftIO $ putStrLn $ "sharePrice: " ++ show sharePrice'
    supply' <- liftWeb3 $ Fund.totalSupply callFund
    liftIO $ putStrLn $ "totalSupply: " ++ show supply'

    forM_ [("MLN-T", mlnToken), ("ETH-T", ethToken), ("EUR-T", eurToken)] $ \(sym, token) -> do
      balance <- liftWeb3 $ Asset.balanceOf defaultCall { callTo = Just token } fund
      liftIO $ putStrLn $ sym ++ "(" ++ show token ++ "): " ++ "balance: " ++ show balance

    liftIO $ putStrLn "done"
    return (fund, canonicalPriceFeed, [mlnToken, ethToken, eurToken])

  case r of
    Left err -> error $ "deploy failed: " ++ show err
    Right x -> return x

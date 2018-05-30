{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Melon.Contract.Version
  ( deploy
  ) where

import Control.Lens
import qualified Data.HashMap.Strict as HashMap
import Control.Monad
import qualified Data.List as List
import qualified Data.Text as T
import Data.Proxy (Proxy (..))
import Network.Ethereum.ABI.Prim.Address (Address)
import Network.Ethereum.Web3.Provider
import Network.Ethereum.Web3.Types

import qualified Melon.ABI.Compliance.NoCompliance as NoCompliance
import qualified Melon.ABI.Exchange.ThirdParty.MatchingMarket as MatchingMarket
import qualified Melon.ABI.Exchange.ThirdParty.SimpleMarket as SimpleMarket
-- import qualified Melon.ABI.Exchange.Adapter.CentralizedAdapter as CentralizedAdapter
import qualified Melon.ABI.Exchange.Adapter.MatchingMarketAdapter as MatchingMarketAdapter
import qualified Melon.ABI.Exchange.Adapter.SimpleAdapter as SimpleAdapter
-- import qualified Melon.ABI.FundRanking as FundRanking
import qualified Melon.ABI.PriceFeeds.CanonicalPriceFeed as CanonicalPriceFeed
import qualified Melon.ABI.RiskMgmt.RMMakeOrders as RMMakeOrders
import qualified Melon.ABI.System.Governance as Governance
import qualified Melon.ABI.Version.Version as Version
import Melon.Context
import qualified Melon.Contract.Governance as Governance
import qualified Melon.Contract.PreminedAsset as PreminedAsset
import qualified Melon.Contract.PriceFeed as PriceFeed
import Melon.Model
import Melon.ThirdParty.Network.Ethereum.ABI.Codec
import Melon.ThirdParty.Network.Ethereum.Web3.Eth


-- | Deploy version contract and surrounding modules.
deploy
  :: Address
    -- ^ Owner
  -> MelonT Web3 VersionDeployment
    -- ^ Returns deployment
deploy owner = do
  defaultCall <- view ctxCall
  let ownerCall = defaultCall { callFrom = Just owner }

  ------------------------------------------------------------
  -- Assets
  assets <- fmap HashMap.fromList $ forM assetsToDeploy $ \spec -> do
    addr <- PreminedAsset.deploy owner
    pure (addr, spec)
  let [mlnToken] = HashMap.keys $
        HashMap.filter (\spec -> _asTokenName spec == "MLN-T") assets
      [ethToken] = HashMap.keys $
        HashMap.filter (\spec -> _asTokenName spec == "ETH-T") assets

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
  let ownerCallMatchingMarket =
        ownerCall { callTo = Just matchingMarket }
      tradingPairs = do
        a:bs <- List.tails (HashMap.keys assets)
        b <- bs
        guard (a /= b)
        pure (a, b)
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
  iforM_ assets $ \asset spec -> do
    let register = encodeCall $ CanonicalPriceFeed.RegisterAssetData
          asset -- asset address
          (spec^.asName) -- asset name
          (spec^.asTokenName) -- asset symbol
          (fromIntegral $ spec^.asDecimals) -- asset decimal places
          "" -- asset related URL
          mockBytes -- asset IPFS hash
          mockAddress -- asset break-in address
          mockAddress -- asset break-out address
          [] -- asset EIP standards
          [] -- asset white listed functions
    unless (asset == mlnToken) $
      -- XXX: Why don't we register the melon token?
      Governance.action governance owner canonicalPriceFeed register 0

  ------------------------------------------------------------
  -- Verify version deployment
  let callVersion = defaultCall { callTo = Just $ version }
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

  pure $! VersionDeployment
    { _vdAddress = version
    , _vdOwner = owner
    , _vdMlnToken = mlnToken
    , _vdEthToken = ethToken
    , _vdAssets = assets
    , _vdGovernance = governance
    , _vdCanonicalPriceFeed = canonicalPriceFeed
    , _vdStakingPriceFeed = stakingPriceFeed
    , _vdSimpleMarket = MarketDeployment
        { _mdMarket = simpleMarket
        , _mdAdapter = simpleAdapter
        }
    , _vdMatchingMarket = MarketDeployment
        { _mdMarket = matchingMarket
        , _mdAdapter = matchingMarketAdapter
        }
    , _vdCompliance = noCompliance
    , _vdRiskManagement = rmMakeOrders
    }


assetsToDeploy :: [AssetSpec]
assetsToDeploy =
  [ AssetSpec
      { _asName = "Melon token"
      , _asTokenName = "MLN-T"
      , _asCryptoCompareName = "MLN"
      , _asDecimals = 18
      }
  , AssetSpec
      { _asName = "Ether token"
      , _asTokenName = "ETH-T"
      , _asCryptoCompareName = "ETH"
      , _asDecimals = 18
      }
  , AssetSpec
      { _asName = "Euro token"
      , _asTokenName = "EUR-T"
      , _asCryptoCompareName = "EUR"
      , _asDecimals = 8
      }
  ]


mockBytes :: T.Text
mockBytes = "0x86b5eed81db5f691c36cc83eb58cb5205bd2090bf3763a19f0c5bf2f074dd84b"


mockAddress :: Address
mockAddress = "0x083c41ea13af6c2d5aaddf6e73142eb9a7b00183"

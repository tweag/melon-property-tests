{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Melon.Model.Input where

import Control.Lens.TH (makeClassy)
import Data.HashMap.Strict (HashMap)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.Ethereum.ABI.Prim.Address (Address)
import Network.Ethereum.ABI.Prim.Bytes (BytesN)
import Network.Ethereum.ABI.Prim.Int (UIntN)



data ModelInput = ModelInput
  { _miVersion :: VersionDeployment
    -- ^ The version deployment.
  , _miFund :: FundDeployment
    -- ^ The fund deployment.
  , _miInvestors :: [Address]
    -- ^ Available fund investors.
  } deriving Generic


data FundDeployment = FundDeployment
  { -- | Fund contract address.
    _fdAddress :: Address
  , -- | Fund manager.
    _fdManager :: Address
  , -- | Terms and conditions signature.
    _fdTermsAndConditions :: (BytesN 32, BytesN 32, UIntN 8)
  } deriving (Generic, Show)


data VersionDeployment = VersionDeployment
  { -- | Version contract address.
    _vdAddress :: Address
  , -- | Version contract owner.
    _vdOwner :: Address
  , -- | Melon token.
    _vdMlnToken :: Address
  , -- | Ether token.
    _vdEthToken :: Address
  , -- | Assets associated with this version deployment.
    _vdAssets :: HashMap Address AssetSpec
  , -- | Governance contract.
    _vdGovernance :: Address
  , -- | CanonicalPriceFeed contract.
    _vdCanonicalPriceFeed :: Address
  , -- | StakingPriceFeed contract.
    _vdStakingPriceFeed :: Address
    -- | Simple market.
  , _vdSimpleMarket :: MarketDeployment
    -- | Matching market.
  , _vdMatchingMarket :: MarketDeployment
    -- | Compliance module.
  , _vdCompliance :: Address
    -- | Risk management module.
  , _vdRiskManagement :: Address
  } deriving (Generic, Show)


data MarketDeployment = MarketDeployment
  { -- | Market contract
    _mdMarket :: Address
  , -- | Market adapter contract
    _mdAdapter :: Address
  } deriving (Generic, Show)


data AssetSpec = AssetSpec
  { -- | Asset descriptive name
    _asName :: BytesN 32
  , -- | Asset token name
    _asTokenName :: BytesN 8
  , -- | Asset name on crypto compare
    _asCryptoCompareName :: T.Text
  , -- | Asset's number of decimal digits
    _asDecimals :: Integer
  } deriving (Generic, Show)


makeClassy ''AssetSpec
makeClassy ''MarketDeployment
makeClassy ''VersionDeployment
makeClassy ''FundDeployment
makeClassy ''ModelInput

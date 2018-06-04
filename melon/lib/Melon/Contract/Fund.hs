{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Melon.Contract.Fund
  ( deploy
  ) where

import Control.Lens
import Control.Monad (unless)
import Network.Ethereum.ABI.Prim.Address (Address)
import Network.Ethereum.ABI.Prim.Int (UIntN)
import Network.Ethereum.Web3.Types (Call (..))

import qualified Melon.ABI.Version.Version as Version
import Melon.Context
import Melon.Contract.TermsAndConditions
import Melon.Model.Input
import Melon.ThirdParty.Network.Ethereum.Web3.Eth


deploy
  :: MonadMelon m
  => VersionDeployment
    -- ^ The version contract and modules.
  -> Address
    -- ^ The fund manager.
  -> UIntN 256
    -- ^ The management fee (0 = 0%, 10^18 = 100%)
  -> UIntN 256
    -- ^ The performance fee (0 = 0%, 10^18 = 100%)
  -> m FundDeployment
deploy version manager managementFee performanceFee = do
  defaultCall <- getCall
  let managerCall = defaultCall { callFrom = Just manager }
      callVersion = defaultCall { callTo = Just $ version^.vdAddress }
      managerCallVersion = managerCall { callTo = Just $ version^.vdAddress }

  -- XXX: Replace calls to 'error' with dedicated exceptions.

  ------------------------------------------------------------
  -- Setup a fund instance
  (r, s, v) <- liftWeb3 $ getTermsSignatureParameters manager
  fund <- liftWeb3 $ Version.setupFund managerCallVersion
    "Test fund" -- fund name
    (version^.vdMlnToken) -- quote asset
    managementFee -- management fee
    performanceFee -- performance fee
    (version^.vdCompliance) -- participation module address
    (version^.vdRiskManagement) -- risk management module address
    -- addresses of exchanges where the fund can trade
    [ version^.vdSimpleMarket.mdMarket
    , version^.vdMatchingMarket.mdMarket
    ]
    v r s -- signature elliptic curve parameters
    >>= getTransactionEvents >>= \case
    [Version.FundUpdated fund] -> pure fund
    _ -> error "Expected one FundUpdated event"
  fund' <- liftWeb3 $ Version.managerToFunds callVersion manager
  unless (fund' == fund) $
    error "Fund address mismatch"

  pure $! FundDeployment
    { _fdAddress = fund
    , _fdManager = manager
    , _fdTermsAndConditions = (r, s, v)
    }

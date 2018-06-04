{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Melon.Contract.Fund
  ( deploy
  ) where

import Control.Lens
import Control.Monad (unless)
import Network.Ethereum.ABI.Prim.Address (Address)
import Network.Ethereum.Web3.Types (Call (..))

import qualified Melon.ABI.Version.Version as Version
import Melon.Context
import Melon.Contract.TermsAndConditions
import Melon.Model
import Melon.ThirdParty.Network.Ethereum.Web3.Eth


deploy
  :: MonadMelon m
  => VersionDeployment
    -- ^ The version contract and modules.
  -> Address
    -- ^ The fund manager.
  -> m FundDeployment
deploy version manager = do
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
    (10^(16::Int)) -- management fee
    10 -- performance fee
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

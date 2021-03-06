{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Melon.Contract.Governance
  ( deploy
  , action
  ) where

import Control.Exception.Safe (Exception (..), throw)
import Data.Typeable (Typeable)
import Network.Ethereum.ABI.Prim.Address (Address)
import Network.Ethereum.ABI.Prim.Bytes (Bytes)
import Network.Ethereum.ABI.Prim.Int (UIntN)
import Network.Ethereum.Web3.Types (Call (..))

import qualified Melon.ABI.System.Governance as Governance
import Melon.Context
import Melon.ThirdParty.Network.Ethereum.Web3.Eth


deploy
  :: MonadMelon m
  => Address
    -- ^ Owner
  -> m Address
    -- ^ Returns the contract address
deploy owner = do
  defaultCall <- getCall
  let ownerCall = defaultCall { callFrom = Just owner }
      authorities = [owner]
      quorum = 1
      window = 100000
  tx <- liftWeb3 $ Governance.constructor ownerCall authorities quorum window
  liftWeb3 $ getContractAddress tx


-- | Propose, confirm, and trigger governance action.
-- Only works with quorum of one.
action
  :: MonadMelon m
  => Address -- ^ Governance contract address
  -> Address -- ^ Single quorum governor
  -> Address -- ^ Target contract address
  -> Bytes -- ^ Encoded call data
  -> UIntN 256 -- ^ Call value
  -> m ()
action governance governor target calldata value = do
  defaultCall <- getCall
  let governorCallGovernance = defaultCall
        { callFrom = Just governor
        , callTo = Just governance
        }
  liftWeb3 $ do
    proposeTx <- Governance.propose governorCallGovernance
      target calldata value
    proposalId <- getTransactionEvents proposeTx >>= \case
      [Governance.Proposed proposalId calldata']
        | calldata' == calldata -> return proposalId
        | otherwise -> throw ProposedCallDataMismatch
      [] -> throw NoProposedEvent
      _ -> throw TooManyProposedEvents
    confirmTx <- Governance.confirm governorCallGovernance
      proposalId
    getTransactionEvents confirmTx >>= \case
      [Governance.Confirmed proposalId' _]
        | proposalId' == proposalId -> return ()
        | otherwise -> throw ConfirmedProposalIdMismatch
      [] -> throw NoConfirmedEvent
      _ -> throw TooManyConfirmedEvents
    triggerTx <- Governance.trigger governorCallGovernance
      proposalId
    getTransactionEvents triggerTx >>= \case
      [Governance.Triggered proposalId']
        | proposalId' == proposalId -> return ()
        | otherwise -> throw TriggeredProposalIdMismatch
      [] -> throw NoTriggeredEvent
      _ -> throw TooManyTriggeredEvents


-- | Errors occurring during governance actions.
data GovernanceError
  = NoProposedEvent
    -- ^ A proposal did not cause a 'Melon.ABI.System.Governance.Proposed'
    -- event to be generated.
  | TooManyProposedEvents
    -- ^ A proposal caused more than one 'Melon.ABI.System.Governance.Proposed'
    -- event to be generated.
  | ProposedCallDataMismatch
    -- ^ The result 'Melon.ABI.System.Governance.Proposed' event's call-data
    -- doesn't match the original request's call-data.
  | NoConfirmedEvent
    -- ^ A confirm did not cause a 'Melon.ABI.System.Governance.Confirmed'
    -- event to be generated when expected.
  | TooManyConfirmedEvents
    -- ^ A confirm caused more than one 'Melon.ABI.System.Governance.Confirmed'
    -- event to be generated.
  | ConfirmedProposalIdMismatch
    -- ^ The result 'Melon.ABI.System.Governance.Confirmed' event's proposal Id
    -- doesn't match the request's proposal Id.
  | NoTriggeredEvent
    -- ^ A trigger did not cause a 'Melon.ABI.System.Governance.Triggered'
    -- event to be generated when expected.
  | TooManyTriggeredEvents
    -- ^ A trigger caused more than one 'Melon.ABI.System.Governance.Triggered'
    -- event to be generated.
  | TriggeredProposalIdMismatch
    -- ^ The result 'Melon.ABI.System.Governance.Triggered' event's proposal Id
    -- doesn't match the request's proposal Id.
  deriving (Show, Typeable)
instance Exception GovernanceError where
  displayException = \case
    NoProposedEvent ->
      "A proposal did not cause a `Proposed` event to be generated."
    TooManyProposedEvents ->
      "A proposal caused more than one `Proposed` event to be generated."
    ProposedCallDataMismatch ->
      "The call-data in the `Proposed` event did not match that of the\
      \ original request."
    NoConfirmedEvent ->
      "A confirm did not cause a `Confirmed` event to be generated\
      \ when expected."
    TooManyConfirmedEvents ->
      "A confirm caused more than one `Confirmed` event to be generated."
    ConfirmedProposalIdMismatch ->
      "The result `Confirmed` event's proposal Id doesn't match the request's\
      \ proposal Id."
    NoTriggeredEvent ->
      "A trigger did not cause a `Triggered` event to be generated\
      \ when expected."
    TooManyTriggeredEvents ->
      "A trigger caused more than one `Triggered` event to be generated."
    TriggeredProposalIdMismatch ->
      "The result `Triggered` event's proposal Id doesn't match the request's\
      \ proposal Id."

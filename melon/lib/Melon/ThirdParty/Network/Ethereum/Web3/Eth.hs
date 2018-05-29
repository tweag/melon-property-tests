{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Melon.ThirdParty.Network.Ethereum.Web3.Eth
  ( getTransactionReceipt'
  , getContractAddress
  , getTransactionEvents
  , TransactionError (..)
  ) where

import Control.Exception.Safe (Exception (..), throw)
import Data.Default (Default)
import Data.Typeable (Typeable)
import Network.Ethereum.ABI.Event (DecodeEvent)
import Network.Ethereum.ABI.Prim.Address (Address)
import Network.Ethereum.Web3.Eth (getTransactionReceipt)
import Network.Ethereum.Web3.Provider (Web3)
import Network.Ethereum.Web3.Types (Filter, Hash, TxReceipt (..))

import Melon.ThirdParty.Network.Ethereum.ABI.Event (decodeEvents')


-- | Failures related to transactions.
data TransactionError
  = NoTransactionReceipt Hash
    -- ^ No transaction receipt was available.
  | NoContractAddress Hash
    -- ^ The transaction did not return a contract address.
  deriving (Show, Typeable)
instance Exception TransactionError where
  displayException = \case
    NoTransactionReceipt tx ->
      "No transaction receipt available for transaction hash "
      ++ show tx ++ "."
    NoContractAddress tx ->
      "No contract address available for transaction hash "
      ++ show tx ++ "."


-- | Retrieve the 'Network.Ethereum.Web3.Types.TxReceipt' to the given
-- transaction hash. Throw an exception if it is unavailable.
getTransactionReceipt' :: Hash -> Web3 TxReceipt
getTransactionReceipt' tx =
  getTransactionReceipt tx
  >>= maybe (throw $ NoTransactionReceipt tx) pure


-- | Retrieve the contract address of the contract deployed during the
-- given transaction. Throw an exception if it is unavailable.
getContractAddress :: Hash -> Web3 Address
getContractAddress tx = do
  mbAddress <- receiptContractAddress <$> getTransactionReceipt' tx
  maybe (throw $ NoContractAddress tx) pure mbAddress


-- | Retrieve the events matching the specified type from the given
-- transaction. Throws an exception if the transaction does not exist, or if
-- the decoding of an event fails. Note, the returned list may be empty.
getTransactionEvents
  :: (DecodeEvent i ni e, Default (Filter e))
  => Hash -> Web3 [e]
getTransactionEvents tx =
  decodeEvents' =<< receiptLogs <$> getTransactionReceipt' tx

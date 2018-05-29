{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Melon.Context
  ( MelonM (..)
  , runMelonM
  , withContext
  , liftWeb3

  , Context (..)
  , ctxCall
  ) where

import Control.Exception.Safe (MonadThrow)
import Control.Lens.TH (makeClassy)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (..), lift)
import Data.Default (def)
import GHC.Generics (Generic)
import Network.Ethereum.Web3.Provider (Web3)
import Network.Ethereum.Web3.Types (Call (..))


newtype MelonM a = MelonM { unMelonM :: ReaderT Context Web3 a }
  deriving
    ( Functor, Applicative, Monad
    , MonadIO, MonadReader Context, MonadThrow
    )

runMelonM :: MelonM a -> Web3 a
runMelonM (MelonM action) = runReaderT action Context
  { _ctxCall = def
      { callGas = Just 6900000
      , callGasPrice = Just 100000000000
      }
  }

withContext :: (Context -> Web3 a) -> MelonM a
withContext = MelonM . ReaderT

liftWeb3 :: Web3 a -> MelonM a
liftWeb3 = MelonM . lift


-- | Common context required for interaction with the Melon contract.
data Context = Context
  { _ctxCall :: Call
  } deriving (Generic, Show)
makeClassy ''Context

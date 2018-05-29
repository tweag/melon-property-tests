{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Melon.Context
  ( MelonT (..)
  , runMelonT
  , withContext
  , liftWeb3
  , hoistWeb3
  , runWeb3Throw

  , Context (..)
  , ctxCall
  ) where

import Control.Exception.Safe (MonadCatch, MonadThrow, throw)
import Control.Lens.TH (makeClassy)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Default (def)
import GHC.Generics (Generic)
import Hedgehog (MonadTest)
import Network.Ethereum.Web3.Provider (Web3, runWeb3)
import Network.Ethereum.Web3.Types (Call (..))


newtype MelonT m a = MelonT { unMelonT :: ReaderT Context m a }
  deriving
    ( Functor, Applicative, Monad, MonadTrans, MFunctor
    , MonadIO, MonadReader Context, MonadCatch, MonadThrow, MonadTest
    )

runMelonT :: MelonT m a -> m a
runMelonT (MelonT action) = runReaderT action Context
  { _ctxCall = def
      { callGas = Just 6900000
      , callGasPrice = Just 100000000000
      }
  }

withContext :: (Context -> m a) -> MelonT m a
withContext = MelonT . ReaderT

liftWeb3 :: Web3 a -> MelonT Web3 a
liftWeb3 = lift

runWeb3Throw :: (MonadIO m, MonadThrow m) => Web3 a -> m a
runWeb3Throw = either throw return <=< runWeb3

hoistWeb3 :: (MonadIO m, MonadThrow m) => MelonT Web3 a -> MelonT m a
hoistWeb3 = hoist runWeb3Throw


-- | Common context required for interaction with the Melon contract.
data Context = Context
  { _ctxCall :: Call
  } deriving (Generic, Show)
makeClassy ''Context

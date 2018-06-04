{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Melon.Context
  ( MelonT (..)
  , runMelonT
  , withContext

  , MonadWeb3 (..)
  , MonadMelon (..)

  , Context (..)
  , ctxCall
  , ctxProvider
  , ctxManager

  , Manager
  , Provider
  , getProvider
  ) where

import Control.Exception.Safe (MonadCatch, MonadThrow, throw)
import Control.Lens (view)
import Control.Lens.TH (makeClassy)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Default (def)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Hedgehog (MonadTest)
import Network.Ethereum.Web3.Provider (JsonRpcProvider (..), Provider (..), Web3, runWeb3With)
import Network.Ethereum.Web3.Types (Call (..))
import Network.HTTP.Client (Manager)
import System.Environment (lookupEnv)


-- | Common context required for interaction with the Melon contract.
data Context = Context
  { _ctxCall :: Call
  , _ctxProvider :: Provider
  , _ctxManager :: Manager
  } deriving Generic
makeClassy ''Context


newtype MelonT m a = MelonT { unMelonT :: ReaderT Context m a }
  deriving
    ( Functor, Applicative, Monad, MonadTrans, MFunctor
    , MonadIO, MonadReader Context, MonadCatch, MonadThrow, MonadTest
    )

runMelonT :: Manager -> Provider -> MelonT m a -> m a
runMelonT manager provider (MelonT action) = runReaderT action Context
  { _ctxCall = def
      { callGas = Just 6900000
      , callGasPrice = Just 100000000000
      }
  , _ctxProvider = provider
  , _ctxManager = manager
  }

withContext :: (Context -> m a) -> MelonT m a
withContext = MelonT . ReaderT


class MonadIO m => MonadWeb3 m where
  liftWeb3 :: Web3 a -> m a
instance (MonadIO m, MonadThrow m) => MonadWeb3 (MelonT m) where
  liftWeb3 action = do
    manager <- view ctxManager
    provider <- view ctxProvider
    r <- runWeb3With manager provider action
    case r of
      Left err -> throw err
      Right x -> pure x


class MonadWeb3 m => MonadMelon m where
  getCall :: m Call
instance (MonadIO m, MonadThrow m) => MonadMelon (MelonT m) where
  getCall = view ctxCall


getProvider :: IO Provider
getProvider = do
  uri <- fromMaybe "http://localhost:8545" <$> lookupEnv "WEB3_PROVIDER"
  pure $ def { jsonRpc = HttpProvider uri }

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

  , Manager
  , Provider
  , getProvider
  ) where

import Control.Exception.Safe (MonadCatch, MonadThrow, throw)
import Control.Lens (view)
import Control.Lens.TH (makeClassy)
import Control.Monad ((<=<))
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

liftWeb3 :: Web3 a -> MelonT Web3 a
liftWeb3 = lift

runWeb3Throw
  :: (MonadIO m, MonadThrow m)
  => Manager -> Provider -> Web3 a -> m a
runWeb3Throw manager provider =
  either throw return <=< runWeb3With manager provider

hoistWeb3 :: (MonadIO m, MonadThrow m) => MelonT Web3 a -> MelonT m a
hoistWeb3 action = do
  manager <- view ctxManager
  provider <- view ctxProvider
  hoist (runWeb3Throw manager provider) action


getProvider :: IO Provider
getProvider = do
  uri <- fromMaybe "http://localhost:8545" <$> lookupEnv "WEB3_PROVIDER"
  pure $ def { jsonRpc = HttpProvider uri }

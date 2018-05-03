{-# LANGUAGE LambdaCase #-}

-- | Defines calls to external processes to setup the test environment.
module Melon.External
  ( Config (..)
  , withTestEnv
  , setupTestFund
  ) where

import Control.Exception.Safe (Exception (..), throwIO)
import Control.Monad ((>=>), void)
import Data.Typeable
import System.Exit
import System.IO (Handle)
import System.Process
  ( CreateProcess (..)
  , StdStream (..)
  , proc
  , terminateProcess
  , waitForProcess
  , withCreateProcess
  )


-- | Common options for external processes.
newtype Config = Config
  { cLogFileHandle :: Handle
    -- ^ Log-file handle to write stdout and stderr into.
  }

-- | Failures that can occur during test-env setup.
newtype ExternalError
  = SetupTestFundFailed Int
    -- ^ Failed to setup the test fund.
  deriving (Show, Typeable)

instance Exception ExternalError where
  displayException = \case
    SetupTestFundFailed ec ->
      "Failed to set-up the test fund. Failed with exit-code "
      ++ show ec ++ "."

-- | Start a @npm run devchain@ in the background
-- and execute the given action while it is running.
-- Terminate the devchain once the action completed.
withTestEnv :: Config -> IO a -> IO a
withTestEnv cfg action = withCreateProcess testEnvProc $ \_ _ _ ph -> do
  r <- action
  terminateProcess ph
  void $ waitForProcess ph
  return r
  where
    testEnvProc :: CreateProcess
    testEnvProc = (proc "npm" ["run", "devchain"])
      -- Note the path is relative to the Haskell project root,
      -- i.e. the directory containing the file @melon-property-tests.cabal@.
      { cwd = Just "../smart-contracts"
      , std_out = UseHandle (cLogFileHandle cfg)
      , std_err = UseHandle (cLogFileHandle cfg)
      }

-- | Deploy the test fund.
setupTestFund :: Config -> IO ()
setupTestFund cfg = withCreateProcess setupTestFundProc $ \_ _ _ ->
  waitForProcess >=> \case
    ExitSuccess -> return ()
    ExitFailure ec -> throwIO $ SetupTestFundFailed ec
  where
    setupTestFundProc :: CreateProcess
    setupTestFundProc = (proc "npm" ["run", "setupfund"])
      -- Note the path is relative to the Haskell project root,
      -- i.e. the directory containing the file @melon-property-tests.cabal@.
      { cwd = Just "../smart-contracts"
      , std_out = UseHandle (cLogFileHandle cfg)
      , std_err = UseHandle (cLogFileHandle cfg)
      }

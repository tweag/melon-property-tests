{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Defines calls to external processes to setup the test environment.
module Melon.External where

import Control.Exception.Safe (Exception (..), throwIO)
import Control.Lens
import Control.Monad (void)
import qualified Data.Aeson as Json
import qualified Data.Aeson.Casing as Json
import qualified Data.ByteString.Lazy as LBS
import Data.Typeable
import GHC.Generics
import Network.Ethereum.ABI.Prim.Address
import Path
import System.Exit
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
  { cSmartContractsDir :: Path Abs Dir
    -- ^ Root directory of the @smart-contracts@ repository.
  }

-- | Apply the given configuration to the given process setup.
configure :: Config -> CreateProcess -> CreateProcess
configure cfg createProc = createProc
  { cwd = Just (fromAbsDir (cSmartContractsDir cfg))
  }

-- | Failures that can occur during test-env setup.
data ExternalError
  = SetupTestFundFailed Int
    -- ^ Failed to setup the test fund.
  | ParseTestFundFailed String
    -- ^ Failed to parse JSON output of test fund setup.
  deriving (Show, Typeable)

instance Exception ExternalError where
  displayException = \case
    SetupTestFundFailed ec ->
      "Failed to set-up the test fund. Failed with exit-code "
      ++ show ec ++ "."
    ParseTestFundFailed msg ->
      "Failed to parse result of test fund setup: "
      ++ msg

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
    testEnvProc = configure cfg
      (proc "/usr/bin/env" ["npm", "run", "devchain"])

-- | JSON serialization options.
aesonOptions :: Json.Options
aesonOptions = Json.aesonPrefix Json.snakeCase

-- | An asset on the fund
newtype FundAsset = FundAsset
  { _faAddress :: Address
    -- ^ The address of the asset.
  } deriving (Generic, Show)
instance Json.FromJSON FundAsset where
  parseJSON = Json.genericParseJSON aesonOptions
instance Json.ToJSON FundAsset where
  toJSON = Json.genericToJSON aesonOptions

-- | Outcome of setting up a test fund.
data TestFund = TestFund
  { _tfAddress :: Address
    -- ^ Address of the fund.
  , _tfMlnToken :: FundAsset
    -- ^ Melon token test asset.
  } deriving (Generic, Show)
instance Json.FromJSON TestFund where
  parseJSON = Json.genericParseJSON aesonOptions
instance Json.ToJSON TestFund where
  toJSON = Json.genericToJSON aesonOptions

-- | Deploy the test fund.
setupTestFund :: Config -> IO TestFund
setupTestFund cfg = withCreateProcess setupTestFundProc $
  \_ (Just hStdout) _ ph -> do
    result <- Json.eitherDecode <$> LBS.hGetContents hStdout
    status <- waitForProcess ph
    case (status, result) of
      (ExitFailure ec, _) -> throwIO $ SetupTestFundFailed ec
      (ExitSuccess, Left err) -> throwIO $ ParseTestFundFailed err
      (ExitSuccess, Right json) -> return json
  where
    setupTestFundProc = configure cfg
      (proc "/usr/bin/env" ["npm", "run", "-s", "setupfund"])
      { std_out = CreatePipe }

makeLenses ''FundAsset
makeLenses ''TestFund

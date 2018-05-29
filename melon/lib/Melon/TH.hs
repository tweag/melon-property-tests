{-# LANGUAGE TemplateHaskell #-}

module Melon.TH where

import Data.ByteArray (convert)
import Data.ByteArray.Encoding (Base (Base16), convertFromBase)
import qualified Data.ByteString as BS
import Data.FileEmbed (bsToExp)
import Data.Maybe (fromMaybe)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Network.Ethereum.ABI.Prim.Bytes (Bytes)
import Network.Ethereum.Contract.TH
import Path
import Path.IO
import System.Environment (lookupEnv)


-- | Read the melonport smart-contracts binary directory from the environment
-- or use the default relative to the directory containing `melon.cabal`.
getSmartContractsDir :: Q (Path Abs Dir)
getSmartContractsDir = do
  mbBinDir <- runIO $ lookupEnv "MELONPORT_SMARTCONTRACTS_DIR"
  let binDir = fromMaybe "../smart-contracts/out" mbBinDir
  runIO $ resolveDir' binDir


melonAbiFrom :: QuasiQuoter
melonAbiFrom = QuasiQuoter
  { quotePat = undefined
  , quoteType = undefined
  , quoteExp = \contractName -> do
      contractAbi <- getContractAbi contractName
      quoteExp abi contractAbi
  , quoteDec = \contractName -> do
      contractAbi <- getContractAbi contractName
      contractBin <- getContractBin contractName
      let abiName = mkName "contractAbi"
          binName = mkName "contractBin"
      abiBin <- sequence
        [ sigD abiName [t|String|]
        , funD abiName [clause [] (normalB (quoteExp abi contractAbi)) []]
        , sigD binName [t|Bytes|]
        , funD binName [clause [] (normalB [e|convert $(bsToExp contractBin)|]) []]
        ]
      defs <- quoteDec abi contractAbi
      pure $ abiBin ++ defs
  }
  where
    getContractFileName contractName suffix = do
      outDir <- getSmartContractsDir
      relFileName <- parseRelFile contractName
      fmap fromAbsFile $ (outDir </> relFileName) <.> suffix
    getContractAbi :: String -> Q String
    getContractAbi contractName = do
      fileName <- getContractFileName contractName "abi"
      fileContent <- runIO (readFile fileName)
      addDependentFile fileName
      return fileContent
    getContractBin :: String -> Q BS.ByteString
    getContractBin contractName = do
      fileName <- getContractFileName contractName "bin"
      fileContent <- runIO (BS.readFile fileName)
      let mkerr msg =
            "Expected hex-encoded binary contract in "
            ++ fileName ++ ": " ++ msg
      bytes <- either (fail . mkerr) pure $ convertFromBase Base16 fileContent
      addDependentFile fileName
      return bytes

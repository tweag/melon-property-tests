{-# LANGUAGE QuasiQuotes #-}

module Main
  ( main
  ) where

import Path
import Path.IO

import Melon.External


main :: IO ()
main = do
  cwd <- getCurrentDir
  let logdir = cwd </> [reldir|logs|]
  createDirIfMissing True logdir
  (outfile, outhandle) <- openTempFile logdir "out.log"
  let cfg = Config
        { cSmartContractsDir = cwd </> [reldir|smart-contracts|]
        , cLogFileHandle = outhandle
        }
  withTestEnv cfg $ do
    setupTestFund cfg
    putStrLn "done, bye"

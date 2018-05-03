{-# LANGUAGE QuasiQuotes #-}

module Main
  ( main
  ) where

import Control.Concurrent (threadDelay)
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
        { cSmartContractsDir = parent cwd </> [reldir|smart-contracts|]
        }
  withTestEnv cfg $ do
    threadDelay 10000000
    setupTestFund cfg
    putStrLn "done, bye"

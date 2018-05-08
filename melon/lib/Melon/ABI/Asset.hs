{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Melon.ABI.Asset where

import Network.Ethereum.ABI.Prim.Bool ()
import Network.Ethereum.Contract.TH

-- | Human readable listing of the smart-contract ABI.
--
-- View it using @'putStrLn' 'abiOverview'@.
abiOverview :: String
abiOverview = [abiFrom|../smart-contracts/out/Asset.abi|]

-- Note the path is relative to the Haskell project root,
-- i.e. the directory containing the file @melon.cabal@.
[abiFrom|../smart-contracts/out/Asset.abi|]

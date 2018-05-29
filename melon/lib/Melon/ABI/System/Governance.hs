{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Melon.ABI.System.Governance where

import Data.Semigroup ((<>))
import GHC.Generics (Generic)
import qualified Generics.SOP.Universe
import Network.Ethereum.ABI.Codec (encode)
import Network.Ethereum.ABI.Class (ABIGet, ABIPut, ABIType (isDynamic))
import Network.Ethereum.ABI.Prim.Address (Address)
import Network.Ethereum.ABI.Prim.Int (UIntN)
import Network.Ethereum.Contract.Method (Method (selector))
import Network.Ethereum.Web3.Eth (sendTransaction)
import Network.Ethereum.Web3.Provider (Web3)
import Network.Ethereum.Web3.Types (Call (..), Hash)

import Melon.TH


[melonAbiFrom|system/Governance|]

data Constructor = Constructor [Address] (UIntN 256) (UIntN 256)
  deriving Generic
instance Generics.SOP.Universe.Generic Constructor
instance ABIGet Constructor
instance ABIPut Constructor
instance ABIType Constructor where
  isDynamic = const False
instance Method Constructor where
  selector = mempty

constructor :: Call -> [Address] -> UIntN 256 -> UIntN 256 -> Web3 Hash
constructor call' authorities' quorum' window' =
  sendTransaction call'
    { callData = Just $ contractBin <> encode (Constructor authorities' quorum' window')
    , callValue = Nothing
    }

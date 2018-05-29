{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Melon.ABI.PriceFeeds.StakingPriceFeed where

import Data.Semigroup ((<>))
import qualified Generics.SOP.Universe
import GHC.Generics (Generic)
import Network.Ethereum.ABI.Class (ABIGet, ABIPut, ABIType (isDynamic))
import Network.Ethereum.ABI.Codec (encode)
import Network.Ethereum.ABI.Prim.Address (Address)
import Network.Ethereum.Contract.Method (Method (selector))
import Network.Ethereum.Web3.Eth (sendTransaction)
import Network.Ethereum.Web3.Provider (Web3)
import Network.Ethereum.Web3.Types (Call (..), Hash)

import Melon.TH


[melonAbiFrom|pricefeeds/StakingPriceFeed|]

data Constructor
  = Constructor
      Address
      Address
      Address
  deriving Generic
instance Generics.SOP.Universe.Generic Constructor
instance ABIGet Constructor
instance ABIPut Constructor
instance ABIType Constructor where
  isDynamic = const False
instance Method Constructor where
  selector = mempty

constructor
  :: Call
  -> Address
  -> Address
  -> Address
  -> Web3 Hash
constructor call'
  ofRegistrar'
  ofQuoteAsset'
  ofSuperFeed'
  =
  sendTransaction call'
    { callData = Just $ contractBin <> encode (Constructor
        ofRegistrar'
        ofQuoteAsset'
        ofSuperFeed')
    , callValue = Nothing
    }

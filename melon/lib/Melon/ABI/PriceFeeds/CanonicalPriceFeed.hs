{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Melon.ABI.PriceFeeds.CanonicalPriceFeed where

import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Generics.SOP.Universe
import GHC.Generics (Generic)
import Network.Ethereum.ABI.Class (ABIGet, ABIPut, ABIType (isDynamic))
import Network.Ethereum.ABI.Codec (encode)
import Network.Ethereum.ABI.Prim.Address (Address)
import Network.Ethereum.ABI.Prim.Bytes (BytesN)
import Network.Ethereum.ABI.Prim.Int (UIntN)
import Network.Ethereum.Contract.Method (Method (selector))
import Network.Ethereum.Web3.Eth (sendTransaction)
import Network.Ethereum.Web3.Provider (Web3)
import Network.Ethereum.Web3.Types (Call (..), Hash)

import Melon.TH


[melonAbiFrom|pricefeeds/CanonicalPriceFeed|]

data Constructor
  = Constructor
      Address
      (BytesN 32)
      (BytesN 8)
      (UIntN 256)
      Text
      Text
      Address
      Address
      [UIntN 256]
      [BytesN 4]
      [UIntN 256]
      [UIntN 256]
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
  -> BytesN 32
  -> BytesN 8
  -> UIntN 256
  -> Text
  -> Text
  -> Address
  -> Address
  -> [UIntN 256]
  -> [BytesN 4]
  -> [UIntN 256]
  -> [UIntN 256]
  -> Address
  -> Web3 Hash
constructor call'
  ofQuoteAsset'
  quoteAssetName'
  quoteAssetSymbol'
  quoteAssetDecimals'
  quoteAssetUrl'
  quoteAssetIpfsHash'
  quoteAssetBreakIn'
  quoteAssetBreakOut'
  quoteAssetStandards'
  quoteAssetFunctionSignatures'
  updateInfo'
  stakingInfo'
  ofGovernance'
  =
  sendTransaction call'
    { callData = Just $ contractBin <> encode (Constructor
        ofQuoteAsset'
        quoteAssetName'
        quoteAssetSymbol'
        quoteAssetDecimals'
        quoteAssetUrl'
        quoteAssetIpfsHash'
        quoteAssetBreakIn'
        quoteAssetBreakOut'
        quoteAssetStandards'
        quoteAssetFunctionSignatures'
        updateInfo'
        stakingInfo'
        ofGovernance')
    , callValue = Nothing
    }

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Melon.Contract.TermsAndConditions
  ( getTermsSignatureParameters
  ) where


import Control.Applicative (liftA3)
import Control.Exception.Safe (Exception (..), throw)
import Control.Monad (mzero)
import qualified Data.ByteArray.Parse as BP
import Data.ByteArray.Sized (sizedByteArray)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import GHC.TypeNats (KnownNat, natVal)
import Network.Ethereum.ABI.Prim.Bytes (Bytes, BytesN)
import Network.Ethereum.ABI.Prim.Address (Address)
import Network.Ethereum.ABI.Prim.Int (UIntN)
import Network.Ethereum.Web3.Eth (sign)
import Network.Ethereum.Web3.Provider (Web3)


data TermsAndConditionsError
  = FailedToParseTermsAndConditionsSignature
  deriving (Show, Typeable)
instance Exception TermsAndConditionsError where
  displayException FailedToParseTermsAndConditionsSignature =
    "Failed to parse terms and conditions signature."


getTermsSignatureParameters :: Address -> Web3 (BytesN 32, BytesN 32, UIntN 8)
getTermsSignatureParameters manager = do
  sig <- sign manager termsAndConditionsHash
  case BP.parse parseSig sig of
    BP.ParseOK "" parms -> pure parms
    _ -> throw FailedToParseTermsAndConditionsSignature
  where
    termsAndConditionsHash =
      "0xAA9C907B0D6B4890E7225C09CBC16A01CB97288840201AA7CDCB27F4ED7BF159"
    parseBytesN :: forall n. KnownNat n => BP.Parser Bytes (BytesN n)
    parseBytesN = do
      bs <- BP.take (fromIntegral $ natVal (Proxy @n))
      case sizedByteArray bs of
        Nothing -> mzero
        Just bs' -> pure bs'
    parseUInt8 :: BP.Parser Bytes (UIntN 8)
    parseUInt8 = fromIntegral <$> BP.anyByte
    parseSig :: BP.Parser Bytes (BytesN 32, BytesN 32, UIntN 8)
    parseSig = liftA3 (,,) parseBytesN parseBytesN parseUInt8

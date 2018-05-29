{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Melon.ThirdParty.Network.Ethereum.ABI.Codec
  ( encodeCall
  , encodeSignature
  ) where

import Data.ByteArray.Sized (unsafeSizedByteArray)
import Data.Proxy (Proxy (..))
import Data.Semigroup ((<>))
import Network.Ethereum.ABI.Class (ABIPut)
import Network.Ethereum.ABI.Codec (encode)
import Network.Ethereum.ABI.Prim.Bytes (Bytes, BytesN)
import Network.Ethereum.Contract.Method (Method (..))


-- | Encode an Ethereum contract method call,
-- including selector and arguments.
--
-- Use the generated accompanying call data constructor.
--
-- E.g. If a contract @MyContract@ has a method @myMethod(bool, uint)@
-- then you can encode a call like so
--
-- @
--   encodeCall (MyContract.MyMethodData True 42)
-- @
encodeCall :: forall a. (ABIPut a, Method a) => a -> Bytes
encodeCall calldata = selector (Proxy @a) <> encode calldata


-- | Encode an Ethereum contract method signature.
--
-- Use the generated accompanying call data type.
--
-- E.g. If a contract @MyContract@ has a method @myMethod(bool, uint)@
-- then you can encode its signature like so
--
-- @
--   encodeSignature (Proxy @MyContract.MyMethodData)
-- @
encodeSignature :: forall a. Method a => Proxy a -> BytesN 4
encodeSignature proxy = unsafeSizedByteArray $ selector proxy

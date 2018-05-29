{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Melon.ThirdParty.Network.Ethereum.ABI.Event
  ( decodeEvents
  , decodeEvents'
  , DecodeEventsError (..)
  ) where

import Control.Exception.Safe (Exception (..), MonadThrow, throw)
import qualified Data.Aeson.Encode.Pretty as Json
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as C8
import Data.Default (Default, def)
import Data.Typeable (Typeable)
import Network.Ethereum.ABI.Event (DecodeEvent (decodeEvent))
import Network.Ethereum.Web3.Types (Change (..), Filter (..))


-- | Errors occurring during event decoding
data DecodeEventsError
  = DecodeEventFailed Change String
    -- ^ Failed to decode an event from the given
    -- 'Network.Ethereum.Web3.Types.Change'.
  deriving (Show, Typeable)
instance Exception DecodeEventsError where
  displayException (DecodeEventFailed change msg) =
    "Failed to decode an event \"" ++ msg ++ "\" from change "
    ++ C8.unpack (LBS.toStrict (Json.encodePretty change))


-- | Decode a list of events of the specified event type @e@
-- given a list of 'Network.Ethereum.Web3.Types.Change's,
-- e.g. obtained from 'Network.Ethereum.Web3.Types.receiptLogs'.
decodeEvents
  :: forall e i ni
   . (DecodeEvent i ni e, Default (Filter e))
  => [Change] -> Either DecodeEventsError [e]
decodeEvents = traverse eventFromChange . filter matchTopic
  where
    Filter {filterTopics} = def @(Filter e)
    matchTopic Change {changeTopics} = case filterTopics of
      Nothing -> True
      Just mask -> and [m == topic | (Just m, topic) <- zip mask changeTopics]
    eventFromChange :: Change -> Either DecodeEventsError e
    eventFromChange change =
      first (DecodeEventFailed change)
      $ decodeEvent change


-- | Version of 'decodeEvents' that throws an exception on decoding error.
decodeEvents'
  :: forall e i ni m
   . (DecodeEvent i ni e, Default (Filter e), MonadThrow m)
  => [Change] -> m [e]
decodeEvents' = either throw pure . decodeEvents

{-# LANGUAGE DeriveGeneric #-}

module JMAP.Types.Request
  ( Request(..)
  , Invocation(..)
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import           Data.Text (Text)
import qualified Data.Vector as V
import           Data.HashMap.Strict (HashMap)
import           GHC.Generics (Generic)
import           JMAP.Types.Base
import           Numeric.Natural (Natural)

-- | The Request object as described in Section 3.3.
data Request = Request
  { using :: ![URI]
    -- ^ The set of capabilities the client wishes to use. The client
    -- MAY include capability identifiers even if the method calls it
    -- makes do not utilise those capabilities. The server advertises
    -- the set of specifications it supports in the Session object
    -- (see Section 2), as keys on the "capabilities" property.
  , methodCalls :: ![Invocation]
    -- ^ An array of method calls to process on the server. The method
    -- calls MUST be processed sequentially, in order.
  , createdIds :: !(HashMap Id Id)
    -- ^ A map of a (client-specified) creation id to the id the
    -- server assigned when a record was successfully created.
    --
    -- As described later in this specification, some records may have
    -- a property that contains the id of another record. To allow
    -- more efficient network usage, you can set this property to
    -- reference a record created earlier in the same API
    -- request. Since the real id is unknown when the request is
    -- created, the client can instead specify the creation id it
    -- assigned, prefixed with a "#" (see Section 5.3 for more
    -- details).
    --
    -- As the server processes API requests, any time it successfully
    -- creates a new record, it adds the creation id to this map (see
    -- the "create" argument to /set in Section 5.3), with the
    -- server-assigned real id as the value. If it comes across a
    -- reference to a creation id in a create/update, it looks it up
    -- in the map and replaces the reference with the real id, if
    -- found.
    --
    -- The client can pass an initial value for this map as the
    -- "createdIds" property of the Request object. This may be an
    -- empty object. If given in the request, the response will also
    -- include a createdIds property. This allows proxy servers to
    -- easily split a JMAP request into multiple JMAP requests to send
    -- to different servers. For example, it could send the first two
    -- method calls to server A, then the third to server B, before
    -- sending the fourth to server A again. By passing the createdIds
    -- of the previous response to the next request, it can ensure all
    -- of these still resolve. See Section 5.8 for further discussion
    -- of proxy considerations.
  } deriving (Eq, Generic, Show)

instance A.FromJSON Request

instance A.ToJSON Request where
  toEncoding = A.genericToEncoding A.defaultOptions


-- | The Invocation data type as described in Section 3.2. Method
-- calls and responses are represented by the *Invocation* data
-- type. This is a tuple, represented as a JSON array containing three
-- elements:
data Invocation = Invocation
  { name :: !Text
    -- ^ 1. A "String" *name* of the method to call or of the
    -- response.
  , arguments :: !A.Value
    -- ^ 2. A "String[*]" object containing named *arguments* for that
    -- method or response.
  , method_call_id :: !Text
    -- ^ 3. A "String" *method call id*: an arbitrary string from the
    -- client to be echoed back with the responses emitted by that
    -- method call (a method may return 1 or more responses, as it may
    -- make implicit calls to other methods; all responses initiated
    -- by this method call get the same method call id in the
    -- response).
  } deriving (Eq, Generic, Show)

instance A.FromJSON Invocation where
  parseJSON = A.withArray "Invocation" $ \v -> Invocation
    <$> parseIndex v 0
    <*> parseIndex v 1
    <*> parseIndex v 2

instance A.ToJSON Invocation where
  toJSON a = A.Array $ flip V.unfoldr (0 :: Natural) $ \i ->
    case i of
      0 -> Just (A.toJSON $ name a, 1)
      1 -> Just (A.toJSON $ arguments a, 2)
      2 -> Just (A.toJSON $ method_call_id a, 3)
      _ -> Nothing
  toEncoding a = A.foldable
    [ A.toJSON $ name a
    , A.toJSON $ arguments a
    , A.toJSON $ method_call_id a
    ]

parseIndex :: A.FromJSON a => A.Array -> Int -> AT.Parser a
parseIndex v i = case V.indexM v i of
  Nothing -> fail $ "index "<>show i<>" not found"
  Just a -> A.parseJSON a A.<?> AT.Index i

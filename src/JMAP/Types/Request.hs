{-# LANGUAGE DeriveGeneric #-}

module JMAP.Types.Request
  ( Request(..)
  ) where

import qualified Data.Aeson as A
import           Data.HashMap.Strict (HashMap)
import           GHC.Generics (Generic)
import           JMAP.Types.Base

-- FIXME: Spec demands not just JSON, but I-JSON

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
  , createdIds :: !(Maybe (HashMap Id Id))
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

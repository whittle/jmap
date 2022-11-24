{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module JMAP.Server.CoreCapability
  ( coreCapabilityURI
  , CoreCapability(..)
  ) where

import qualified Data.Aeson as A
import qualified Data.Default.Class as D
import           GHC.Generics (Generic)
import           JMAP.Types.Base
import           Refined

coreCapabilityURI :: URI
coreCapabilityURI = "urn:ietf:params:jmap:core"

-- | The required capability for "urn:ietf:params:jmap:core", as
-- described in section 2. The value of this property is an object
-- that MUST contain the following information on server capabilities
-- (suggested minimum values for limits are supplied that allow
-- clients to make efficient use of the network):
data CoreCapability = CoreCapability
  { maxSizeUpload :: !UInt
    -- ^ The maximum file size, in octets, that the server will accept
    -- for a single file upload (for any purpose). Suggested minimum:
    -- 50,000,000.
  , maxConcurrentUpload :: !UInt
    -- ^ The maximum number of concurrent requests the server will
    -- accept to the upload endpoint. Suggested minimum: 4.
  , maxSizeRequest :: !UInt
    -- ^ The maximum size, in octets, that the server will accept for
    -- a single request to the API endpoint. Suggested minimum:
    -- 10,000,000.
  , maxConcurrentRequests :: !UInt
    -- ^ The maximum number of concurrent requests the server will
    -- accept to the API endpoint. Suggested minimum: 4.
  , maxCallsInRequest :: !UInt
    -- ^ The maximum number of method calls the server will accept in
    -- a single request to the API endpoint. Suggested minimum: 16.
  , maxObjectsInGet :: !UInt
    -- ^ The maximum number of objects that the client may request in
    -- a single /get type method call. Suggested minimum: 500.
  , maxObjectsInSet :: !UInt
    -- ^ The maximum number of objects the client may send to create,
    -- update, or destroy in a single /set type method call. This is
    -- the combined total, e.g., if the maximum is 10, you could not
    -- create 7 objects and destroy 6, as this would be 13 actions,
    -- which exceeds the limit. Suggested minimum: 500.
  , collationAlgorithms :: ![Collation]
    -- ^ A list of identifiers for algorithms registered in the
    -- collation registry, as defined in [RFC4790], that the server
    -- supports for sorting when querying records.
  } deriving (Eq, Generic, Show)

instance D.Default CoreCapability where
  def = CoreCapability
    { maxSizeUpload         = UInt $$(refineTH 50_000_000)
    , maxConcurrentUpload   = UInt $$(refineTH 4)
    , maxSizeRequest        = UInt $$(refineTH 10_000_000)
    , maxConcurrentRequests = UInt $$(refineTH 4)
    , maxCallsInRequest     = UInt $$(refineTH 16)
    , maxObjectsInGet       = UInt $$(refineTH 500)
    , maxObjectsInSet       = UInt $$(refineTH 500)
    , collationAlgorithms   = mempty
    }

instance A.FromJSON CoreCapability

instance A.ToJSON CoreCapability where
  toEncoding = A.genericToEncoding A.defaultOptions

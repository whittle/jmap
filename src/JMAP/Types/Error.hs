{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module JMAP.Types.Error
  ( ErrorResponse(..)
    -- * Smart Constructors
    -- $smartConstructors
  , unknownCapability
  , notJSON
  , notRequest
  , limit
  , errorResponse
  , ErrorType(..)
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A (Parser)
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           JMAP.Types.Base (Limit, URI)
import           Network.HTTP.RFC7807
import           Refined (unrefine)

-- | When an HTTP error response is returned to the client, the server
-- SHOULD return a JSON "problem details" object as the response body,
-- as per [RFC7807]. (§ 3.6.1)
newtype ErrorResponse
  = ErrorResponse (Rfc7807Error ErrorType () ())
  deriving (Eq, Generic, Show)

instance A.FromJSON ErrorResponse where
  parseJSON = A.withObject "ErrorResponse" $ \o ->
    fmap ErrorResponse $ Rfc7807Error
    <$> parseET o
    <*> o A..:? "title"
    <*> o A..:? "status"
    <*> o A..:? "detail"
    <*> o A..:? "instance"
    <*> o A..:? "error"
    <*> o A..:? "context"

instance A.ToJSON ErrorResponse where
  toJSON = A.Object . erPairs
  toEncoding = A.pairs . erPairs


-- | Helper: creates the key-value pairs used in the JSON
-- representation of 'ErrorResponse'.
erPairs :: forall kv. (A.KeyValue kv, Monoid kv) => ErrorResponse -> kv
erPairs (ErrorResponse a) = toKeyValue def a <> etPairs (type_ a)
  where def = defaultEncodingOptions

-- $smartConstructors
--
-- There is one smart constructor for each constructor of 'ErrorType'.

-- | Smart constructor for an 'ErrorResponse' with URN
-- "urn:ietf:params:jmap:error:unknownCapability".
unknownCapability :: URI -> ErrorResponse
unknownCapability u = errorResponse $ UnknownCapability u

-- | Smart constructor for an 'ErrorResponse' with URN
-- "urn:ietf:params:jmap:error:notJSON"
notJSON :: ErrorResponse
notJSON = errorResponse NotJSON

-- | Smart constructor for an 'ErrorResponse' with URN
-- "urn:ietf:params:jmap:error:notRequest"
notRequest :: ErrorResponse
notRequest = errorResponse NotRequest

-- | Smart constructor for an 'ErrorResponse' with URN
-- "urn:ietf:params:jmap:error:limit"
limit :: Limit -> ErrorResponse
limit l = errorResponse $ Limit l

-- | A general smart constructor for an 'ErrorResponse'; it fills in
-- all the fields that can be directly deduced from the 'ErrorType'
-- argument.
errorResponse :: ErrorType -> ErrorResponse
errorResponse a = ErrorResponse $ (rfc7807Error a)
  { title = Just $ errorTypeTitle a
  , detail = errorTypeDetail a
  }


-- | 'ErrorType' captures the minimum information necessary to convey
-- a particular JMAP error. The error types are defined in § 3.6.1.
data ErrorType
  = UnknownCapability !URI
    -- ^ The client included a capability in the "using" property of
    -- the request that the server does not support.
  | NotJSON
    -- ^ The content type of the request was not "application/json" or
    -- the request did not parse as I-JSON.
  | NotRequest
    -- ^ The request parsed as JSON but did not match the type
    -- signature of the Request object.
  | Limit !Limit
    -- ^ The request was not processed as it would have exceeded one
    -- of the request limits defined on the capability object, such as
    -- maxSizeRequest, maxCallsInRequest, or maxConcurrentRequests. A
    -- "limit" property MUST also be present on the "problem details"
    -- object, containing the name of the limit being applied.
  deriving (Eq, Generic, Show)

instance A.ToJSON ErrorType where
  toJSON = A.toJSON . errorTypeURN
  toEncoding = A.toEncoding . errorTypeURN


-- | Helper: creates the key-value pairs used in the JSON
-- representation of 'ErrorResponse' that are specific to a particular
-- 'ErrorType'.
etPairs :: forall kv. (A.KeyValue kv, Monoid kv) => ErrorType -> kv
etPairs (UnknownCapability u) = "unknownCapability" A..= u
etPairs (Limit l) = "limit" A..= l
etPairs _ = mempty

-- | Helper: parses an 'ErrorType' out of the same set of key-value
-- pairs that encode an 'ErrorResponse'.
parseET :: A.Object -> A.Parser ErrorType
parseET o = o A..: "type" >>= \case
  "urn:ietf:params:jmap:error:unknownCapability" ->
    UnknownCapability <$> o A..: "unknownCapability"
  "urn:ietf:params:jmap:error:notJSON" ->
    pure NotJSON
  "urn:ietf:params:jmap:error:notRequest" ->
    pure NotRequest
  "urn:ietf:params:jmap:error:limit" ->
    Limit <$> o A..: "limit"
  u -> fail $ "Did not recognize error type '"<>T.unpack u<>"'"

-- | Projects 'ErrorType' onto the "type" field of 'ErrorResponse'.
errorTypeURN :: ErrorType -> URI
errorTypeURN (UnknownCapability _) =
  "urn:ietf:params:jmap:error:unknownCapability"
errorTypeURN NotJSON =
  "urn:ietf:params:jmap:error:notJSON"
errorTypeURN NotRequest =
  "urn:ietf:params:jmap:error:notRequest"
errorTypeURN (Limit _) =
  "urn:ietf:params:jmap:error:limit"

-- | Projects 'ErrorType' onto the "title" field of 'ErrorResponse'.
errorTypeTitle :: ErrorType -> Text
errorTypeTitle (UnknownCapability _) =
  "Requested capability is not known to the server"
errorTypeTitle NotJSON =
  "Server was unable to parse request as I-JSON"
errorTypeTitle NotRequest =
  "Server did not recognize JSON document as a Request object"
errorTypeTitle (Limit _) =
  "Server did not process request as it would exceed limit"

-- | Projects 'ErrorType' onto the "detail" field of 'ErrorResponse'.
errorTypeDetail :: ErrorType -> Maybe Text
errorTypeDetail (UnknownCapability u) = Just $ "The client included the capability '"<>u<>"' in the 'using' property of the request. The server does not support that capability."
errorTypeDetail NotJSON = Just "The body of the request was either not a valid JSON document as defined in RFC-8259, or does not obey the conventions of the Internet JSON (I-JSON) format as defined in RFC-7493."
errorTypeDetail NotRequest = Just "The server recognized the body of the request as valid I-JSON, but didn’t recognize that JSON document as a Request object as defined in the JMAP spec (RFC-8620 § 3.3)."
errorTypeDetail (Limit l) = Just $ "The request was not processed as it would have exceeded the '"<>unrefine l<>"' limit of the capability 'urn:ietf:params:jmap:core'. Capabilities and their limits may be found in the Session object returned from the server in accordance with the JMAP spec (RFC-8620 § 2)."

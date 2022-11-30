{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JMAP.Types.ErrorSpec (spec) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AK
import           JMAP.Types.Arbitrary ()
import           JMAP.Types.Error
import           Refined (unrefine)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "ErrorResponse" $ do
    context "FromJSON" $ do
      prop "round trips" $ \(a :: ErrorResponse)
        -> A.eitherDecode (A.encode a) === Right a

  describe "unknownCapability" $ do
    it "has all the common fields" $ do
      u <- generate arbitrary
      let A.Object o = A.toJSON $ unknownCapability u
      AK.lookup "type" o `shouldBe`
        Just (A.String "urn:ietf:params:jmap:error:unknownCapability")
      AK.lookup "title" o `shouldBe`
        Just (A.String "Requested capability is not known to the server")
      AK.lookup "detail" o `shouldBe`
        Just (A.String $ "The client included the capability '"<>u<>"' in the 'using' property of the request. The server does not support that capability.")
    it "has an unknownCapability field" $ do
      u <- generate arbitrary
      let A.Object o = A.toJSON $ unknownCapability u
      AK.lookup "unknownCapability" o `shouldBe` Just (A.String u)

  describe "notJSON" $ do
    it "has all the common fields" $ do
      let A.Object o = A.toJSON notJSON
      AK.lookup "type" o `shouldBe`
        Just (A.String "urn:ietf:params:jmap:error:notJSON")
      AK.lookup "title" o `shouldBe`
        Just (A.String "Server was unable to parse request as I-JSON")
      AK.lookup "detail" o `shouldBe`
        Just (A.String "The body of the request was either not a valid JSON document as defined in RFC-8259, or does not obey the conventions of the Internet JSON (I-JSON) format as defined in RFC-7493.")

  describe "notRequest" $ do
    it "has all the common fields" $ do
      let A.Object o = A.toJSON notRequest
      AK.lookup "type" o `shouldBe`
        Just (A.String "urn:ietf:params:jmap:error:notRequest")
      AK.lookup "title" o `shouldBe`
        Just (A.String "Server did not recognize JSON document as a Request object")
      AK.lookup "detail" o `shouldBe`
        Just (A.String "The server recognized the body of the request as valid I-JSON, but didn’t recognize that JSON document as a Request object as defined in the JMAP spec (RFC-8620 § 3.3).")

  describe "limit" $ do
    it "has all the common fields" $ do
      l <- generate arbitrary
      let A.Object o = A.toJSON $ limit l
      AK.lookup "type" o `shouldBe`
        Just (A.String "urn:ietf:params:jmap:error:limit")
      AK.lookup "title" o `shouldBe`
        Just (A.String "Server did not process request as it would exceed limit")
      AK.lookup "detail" o `shouldBe`
        Just (A.String $ "The request was not processed as it would have exceeded the '"<>unrefine l<>"' limit of the capability 'urn:ietf:params:jmap:core'. Capabilities and their limits may be found in the Session object returned from the server in accordance with the JMAP spec (RFC-8620 § 2).")
    it "has a limit field" $ do
      l <- generate arbitrary
      let A.Object o = A.toJSON $ limit l
      AK.lookup "limit" o `shouldBe` Just (A.String $ unrefine l)

{-
  Example errors from Section 3.6.1.1:

  {
    "type": "urn:ietf:params:jmap:error:unknownCapability",
    "status": 400,
    "detail": "The Request object used capability 'https://example.com/apis/foobar', which is not supported by this server."
  }

  {
    "type": "urn:ietf:params:jmap:error:limit",
    "limit": "maxSizeRequest",
    "status": 400,
    "detail": "The request is larger than the server is willing to process."
  }
-}

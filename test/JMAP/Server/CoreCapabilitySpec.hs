{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JMAP.Server.CoreCapabilitySpec (spec) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AK
import           JMAP.Server.CoreCapability
import           JMAP.Server.CoreCapability.Arbitrary ()
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "CoreCapability" $ do
    context "FromJSON" $ do
      prop "round trips" $ \(a :: CoreCapability)
        -> A.eitherDecode (A.encode a) === Right a
    context "ToJSON" $ do
      it "has the expected keys" $ do
        a <- generate arbitrary
        let A.Object o = A.toJSON a
        AK.lookup "maxSizeUpload" o `shouldBe` Just (A.toJSON $ maxSizeUpload a)
        AK.lookup "maxConcurrentUpload" o `shouldBe` Just (A.toJSON $ maxConcurrentUpload a)
        AK.lookup "maxSizeRequest" o `shouldBe` Just (A.toJSON $ maxSizeRequest a)
        AK.lookup "maxConcurrentRequests" o `shouldBe` Just (A.toJSON $ maxConcurrentRequests a)
        AK.lookup "maxCallsInRequest" o `shouldBe` Just (A.toJSON $ maxCallsInRequest a)
        AK.lookup "maxObjectsInGet" o `shouldBe` Just (A.toJSON $ maxObjectsInGet a)
        AK.lookup "maxObjectsInSet" o `shouldBe` Just (A.toJSON $ maxObjectsInSet a)
        AK.lookup "collationAlgorithms" o `shouldBe` Just (A.toJSON $ collationAlgorithms a)

{-
  Example from Section 2.1:

  {
    "urn:ietf:params:jmap:core": {
      "maxSizeUpload": 50000000,
      "maxConcurrentUpload": 8,
      "maxSizeRequest": 10000000,
      "maxConcurrentRequest": 8,
      "maxCallsInRequest": 32,
      "maxObjectsInGet": 256,
      "maxObjectsInSet": 128,
      "collationAlgorithms": [
        "i;ascii-numeric",
        "i;ascii-casemap",
        "i;unicode-casemap"
      ]
    }
  }
-}

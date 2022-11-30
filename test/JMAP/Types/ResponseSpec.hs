{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JMAP.Types.ResponseSpec (spec) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AK
import           JMAP.Types.Arbitrary ()
import           JMAP.Types.Response
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "Response" $ do
    context "FromJSON" $ do
      prop "round trips" $ \(a :: Response)
        -> A.eitherDecode (A.encode a) === Right a
    context "ToJSON" $ do
      it "has the expected keys" $ do
        a <- generate arbitrary
        let A.Object o = A.toJSON a
        AK.lookup "methodResponses" o `shouldBe` Just (A.toJSON $ methodResponses a)
        AK.lookup "createdIds" o `shouldBe` Just (A.toJSON $ createdIds a)
        AK.lookup "sessionState" o `shouldBe` Just (A.toJSON $ sessionState a)

{-
  Example response from Section 3.4.1:

  {
    "methodResponses": [
      [ "method1", {
        "arg1": 3,
        "arg2": "foo"
      }, "c1" ],
      [ "method2", {
        "isBlah": true
      }, "c2" ],
      [ "anotherResponseFromMethod2", {
        "data": 10,
        "yetmoredata": "Hello"
      }, "c2"],
      [ "error", {
        "type":"unknownMethod"
      }, "c3" ]
    ],
    "sessionState": "75128aab4b1b"
  }
-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JMAP.Types.RequestSpec (spec) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AK
import           JMAP.Types.Arbitrary ()
import           JMAP.Types.Request
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "Request" $ do
    context "FromJSON" $ do
      prop "round trips" $ \(a :: Request)
        -> A.eitherDecode (A.encode a) === Right a
    context "ToJSON" $ do
      it "has the expected keys" $ do
        a <- generate arbitrary
        let A.Object o = A.toJSON a
        AK.lookup "using" o `shouldBe` Just (A.toJSON $ using a)
        AK.lookup "methodCalls" o `shouldBe` Just (A.toJSON $ methodCalls a)
        AK.lookup "createdIds" o `shouldBe` Just (A.toJSON $ createdIds a)

{-
  Example request from Section 3.3.1:

  {
    "using": [ "urn:ietf:params:jmap:core", "urn:ietf:params:jmap:mail" ],
    "methodCalls": [
      [ "method1", {
        "arg1": "arg1data",
        "arg2": "arg2data"
      }, "c1" ],
      [ "method2", {
        "arg1": "arg1data"
      }, "c2" ],
      [ "method3", {}, "c3" ]
    ]
  }
-}

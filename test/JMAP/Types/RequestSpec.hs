{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JMAP.Types.RequestSpec (spec) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AK
import qualified Data.Vector as V
import           JMAP.Types.Request
import           JMAP.Types.Request.Arbitrary ()
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "Request" $ do
    context "ToJSON" $ do
      prop "round trips" $ \(a :: Request)
        -> A.decode (A.encode a) === Just a
      it "has the expected keys" $ do
        a <- generate arbitrary
        let A.Object o = A.toJSON a
        AK.lookup "using" o `shouldBe` Just (A.toJSON $ using a)
        AK.lookup "methodCalls" o `shouldBe` Just (A.toJSON $ methodCalls a)
        AK.lookup "createdIds" o `shouldBe` Just (A.toJSON $ createdIds a)

  describe "Invocation" $ do
    context "ToJSON" $ do
      prop "round trips" $ \(a :: Invocation)
        -> A.decode (A.encode a) === Just a
      it "encodes as a 3-element JSON Array" $ do
        a <- generate arbitrary
        let A.Array v = A.toJSON a
        V.length v `shouldBe` 3
        V.unsafeIndex v 0 `shouldBe` A.toJSON (name a)
        V.unsafeIndex v 1 `shouldBe` A.toJSON (arguments a)
        V.unsafeIndex v 2 `shouldBe` A.toJSON (method_call_id a)

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JMAP.Types.BaseSpec (spec) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.Types as AT
import qualified Data.Text as T
import qualified Data.Vector as V
import           JMAP.Types.Arbitrary ()
import           JMAP.Types.Base
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (isSuccess)

spec :: Spec
spec = do
  describe "Id" $ do
    context "FromJSON" $ do
      prop "round trips" $ \(a :: Id)
        -> A.eitherDecode (A.encode a) === Right a
      it "rejects empty strings" $ do
        let r = A.fromJSON (A.String "") :: A.Result Id
        r `shouldNotSatisfy` isSuccess
      it "rejects any string longer than 255 characters" $ do
        let r = A.fromJSON (A.String $ T.replicate 256 "a") :: A.Result Id
        r `shouldNotSatisfy` isSuccess
      it "rejects characters outside base64url" $ do
        let r1 = A.fromJSON (A.String "=") :: A.Result Id
        r1 `shouldNotSatisfy` isSuccess
        let r2 = A.fromJSON (A.String ".com") :: A.Result Id
        r2 `shouldNotSatisfy` isSuccess
        let r3 = A.fromJSON (A.String "recherché") :: A.Result Id
        r3 `shouldNotSatisfy` isSuccess
        let r4 = A.fromJSON (A.String "a lot") :: A.Result Id
        r4 `shouldNotSatisfy` isSuccess
    context "FromJSONKey" $ do
      prop "round trips" $ \(a :: Id)
        -> let A.ToJSONKeyText f _ = A.toJSONKey
               A.FromJSONKeyTextParser g = A.fromJSONKey
           in AT.parse g (AK.toText $ f a) === A.Success a
      it "rejects empty strings" $ do
        let A.FromJSONKeyTextParser f = A.fromJSONKey
            r = AT.parse f "" :: A.Result Id
        r `shouldNotSatisfy` isSuccess
      it "rejects any string longer than 255 characters" $ do
        let A.FromJSONKeyTextParser f = A.fromJSONKey
            r = AT.parse f (T.replicate 256 "a") :: A.Result Id
        r `shouldNotSatisfy` isSuccess
      it "rejects characters outside base64url" $ do
        let A.FromJSONKeyTextParser f = A.fromJSONKey
            r1 = AT.parse f "base64=" :: A.Result Id
        r1 `shouldNotSatisfy` isSuccess
        let r2 = AT.parse f "ietf.org" :: A.Result Id
        r2 `shouldNotSatisfy` isSuccess
        let r3 = AT.parse f "Mëtal" :: A.Result Id
        r3 `shouldNotSatisfy` isSuccess
        let r4 = AT.parse f "S P A A A A C E" :: A.Result Id
        r4 `shouldNotSatisfy` isSuccess
    context "ToJSON" $ do
      it "encodes to a JSON String" $ do
        a <- generate (arbitrary :: Gen Id)
        A.toJSON a `shouldSatisfy` isString
    context "ToJSONKey" $ do
      it "encodes in a text-like way" $ do
        a <- generate (arbitrary :: Gen Id)
        let A.ToJSONKeyText f _ = A.toJSONKey
        AK.toText (f a) `shouldBe` fromId a

  describe "UInt" $ do
    context "Bounded" $ do
      it "has correct lower bound" $ do
        fromUInt minBound `shouldBe` (0 :: Integer)
      it "has correct upper bound" $ do
        fromUInt maxBound `shouldBe` (9007199254740989 :: Integer)
    context "FromJSON" $ do
      prop "round trips" $ \(a :: UInt)
        -> A.eitherDecode (A.encode a) === Right a
      it "rejects negative numbers" $ do
        let r = A.fromJSON (A.Number (-1)) :: A.Result UInt
        r `shouldNotSatisfy` isSuccess
      it "rejects numbers above the cap" $ do
        let r = A.fromJSON (A.Number 9007199254740990) :: A.Result UInt
        r `shouldNotSatisfy` isSuccess
    context "ToJSON" $ do
      it "encodes to a JSON Number" $ do
        a <- generate (arbitrary :: Gen UInt)
        A.toJSON a `shouldSatisfy` isNumber

  describe "Invocation" $ do
    context "FromJSON" $ do
      prop "round trips" $ \(a :: Invocation)
        -> A.eitherDecode (A.encode a) === Right a
    context "ToJSON" $ do
      it "encodes as a 3-element JSON Array" $ do
        a <- generate arbitrary
        let A.Array v = A.toJSON a
        V.length v `shouldBe` 3
        V.unsafeIndex v 0 `shouldBe` A.toJSON (name a)
        V.unsafeIndex v 1 `shouldBe` A.toJSON (arguments a)
        V.unsafeIndex v 2 `shouldBe` A.toJSON (method_call_id a)

-- Helpers

isSuccess :: A.Result a -> Bool
isSuccess (A.Error _) = False
isSuccess (A.Success _) = True

isNumber :: A.Value -> Bool
isNumber (A.Number _) = True
isNumber _ = False

isString :: A.Value -> Bool
isString (A.String _) = True
isString _ = False

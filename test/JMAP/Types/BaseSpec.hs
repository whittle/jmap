{-# LANGUAGE ScopedTypeVariables #-}

module JMAP.Types.BaseSpec (spec) where

import qualified Data.Aeson as A
import           JMAP.Types.Base
import           JMAP.Types.Base.Arbitrary ()
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (isSuccess)

spec :: Spec
spec = do
  describe "UInt" $ do
    context "Bounded" $ do
      it "has correct lower bound" $ do
        fromUInt minBound `shouldBe` (0 :: Integer)
      it "has correct upper bound" $ do
        fromUInt maxBound `shouldBe` (9007199254740989 :: Integer)
    context "FromJSON" $ do
      it "should reject negative numbers" $ do
        let r = A.fromJSON (A.Number (-1)) :: A.Result UInt
        r `shouldNotSatisfy` isSuccess
      it "should reject numbers above the cap" $ do
        let r = A.fromJSON (A.Number 9007199254740990) :: A.Result UInt
        r `shouldNotSatisfy` isSuccess
    context "ToJSON" $ do
      prop "roundtrips" $ \(a :: UInt)
        -> A.decode (A.encode a) === Just a
      it "encodes to a JSON Number" $ do
        a <- generate (arbitrary :: Gen UInt)
        A.toJSON a `shouldSatisfy` isNumber

-- Helpers

isSuccess :: A.Result a -> Bool
isSuccess (A.Error _) = False
isSuccess (A.Success _) = True

isNumber :: A.Value -> Bool
isNumber (A.Number _) = True
isNumber _ = False

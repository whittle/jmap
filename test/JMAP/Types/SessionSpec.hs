{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JMAP.Types.SessionSpec (spec) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AK
import           JMAP.Types.Session
import           JMAP.Types.Session.Arbitrary ()
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "Session" $ do
    describe "ToJSON" $ do
      it "roundtrips" $ property $ \(s :: Session)
        -> A.decode (A.encode s) === Just s
      it "has the expected keys" $ do
        s <- generate arbitrary
        let A.Object o = A.toJSON s
        (AK.lookup "capabilities" o >>= maybeJSON) `shouldBe` Just (capabilities s)
        (AK.lookup "accounts" o >>= maybeJSON) `shouldBe` Just (accounts s)
        (AK.lookup "primaryAccounts" o >>= maybeJSON) `shouldBe` Just (primaryAccounts s)
        (AK.lookup "username" o >>= maybeJSON) `shouldBe` Just (username s)
        (AK.lookup "apiUrl" o >>= maybeJSON) `shouldBe` Just (apiUrl s)
        (AK.lookup "downloadUrl" o >>= maybeJSON) `shouldBe` Just (downloadUrl s)
        (AK.lookup "uploadUrl" o >>= maybeJSON) `shouldBe` Just (uploadUrl s)
        (AK.lookup "eventSourceUrl" o >>= maybeJSON) `shouldBe` Just (eventSourceUrl s)
        (AK.lookup "state" o >>= maybeJSON) `shouldBe` Just (state s)

maybeJSON :: A.FromJSON a => A.Value -> Maybe a
maybeJSON v = case A.fromJSON v of
  A.Error _ -> Nothing
  A.Success a -> Just a

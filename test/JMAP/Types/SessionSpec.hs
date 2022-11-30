{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JMAP.Types.SessionSpec (spec) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AK
import           GHC.Base (divInt)
import           JMAP.Types.Arbitrary ()
import           JMAP.Types.Session
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "Session" $ do
    context "FromJSON" $ do
      modifyMaxSuccess (flip divInt 10) $
        prop "round trips" $ \(a :: Session)
        -> A.eitherDecode (A.encode a) === Right a
    context "ToJSON" $ do
      it "has the expected keys" $ do
        a <- generate arbitrary
        let A.Object o = A.toJSON a
        AK.lookup "capabilities" o `shouldBe` Just (A.toJSON $ capabilities a)
        AK.lookup "accounts" o `shouldBe` Just (A.toJSON $ accounts a)
        AK.lookup "primaryAccounts" o `shouldBe` Just (A.toJSON $ primaryAccounts a)
        AK.lookup "username" o `shouldBe` Just (A.toJSON $ username a)
        AK.lookup "apiUrl" o `shouldBe` Just (A.toJSON $ apiUrl a)
        AK.lookup "downloadUrl" o `shouldBe` Just (A.toJSON $ downloadUrl a)
        AK.lookup "uploadUrl" o `shouldBe` Just (A.toJSON $ uploadUrl a)
        AK.lookup "eventSourceUrl" o `shouldBe` Just (A.toJSON $ eventSourceUrl a)
        AK.lookup "state" o `shouldBe` Just (A.toJSON $ state a)

  describe "Account" $ do
    context "FromJSON" $ do
      prop "round trips" $ \(a :: Account)
        -> A.eitherDecode (A.encode a) === Right a
    context "ToJSON" $ do
      it "has the expected keys" $ do
        a <- generate arbitrary
        let A.Object o = A.toJSON a
        AK.lookup "name" o `shouldBe` Just (A.toJSON $ name a)
        AK.lookup "isPersonal" o `shouldBe` Just (A.toJSON $ isPersonal a)
        AK.lookup "isReadOnly" o `shouldBe` Just (A.toJSON $ isReadOnly a)
        AK.lookup "accountCapabilities" o `shouldBe`
          Just (A.toJSON $ accountCapabilities a)

{-
  Example session from Section 2.1:

  In the following example Session object, the user has access to
  their own mail and contacts via JMAP, as well as read-only access
  to shared mail from another user. The server is advertising a
  custom "https://example.com/apis/foobar" capability.

  {
    "capabilities": {
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
      },
      "urn:ietf:params:jmap:mail": {}
      "urn:ietf:params:jmap:contacts": {},
      "https://example.com/apis/foobar": {
        "maxFoosFinangled": 42
      }
    },
    "accounts": {
      "A13824": {
        "name": "john@example.com",
        "isPersonal": true,
        "isReadOnly": false,
        "accountCapabilities": {
          "urn:ietf:params:jmap:mail": {
            "maxMailboxesPerEmail": null,
            "maxMailboxDepth": 10,
            ...
          },
          "urn:ietf:params:jmap:contacts": {
            ...
          }
        }
      },
      "A97813": {
        "name": "jane@example.com",
        "isPersonal": false,
        "isReadOnly": true,
        "accountCapabilities": {
          "urn:ietf:params:jmap:mail": {
            "maxMailboxesPerEmail": 1,
            "maxMailboxDepth": 10,
            ...
          }
        }
      }
    },
    "primaryAccounts": {
      "urn:ietf:params:jmap:mail": "A13824",
      "urn:ietf:params:jmap:contacts": "A13824"
    },
    "username": "john@example.com",
    "apiUrl": "https://jmap.example.com/api/",
    "downloadUrl": "https://jmap.example.com
      /download/{accountId}/{blobId}/{name}?accept={type}",
    "uploadUrl": "https://jmap.example.com/upload/{accountId}/",
    "eventSourceUrl": "https://jmap.example.com
      /eventsource/?types={types}&closeafter={closeafter}&ping={ping}",
    "state": "75128aab4b1b"
  }
-}

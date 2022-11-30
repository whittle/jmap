{-# OPTIONS_GHC -Wno-orphans #-}

module JMAP.Types.Arbitrary () where

import qualified Data.Char as C
import qualified Data.Text as T
import           Data.Text.Arbitrary ()
import qualified Generic.Random as R
import           GHC.Data.Maybe (rightToMaybe)
import           JMAP.Types.Base (fromUInt, Id(..), Invocation, UInt(..))
import           JMAP.Types.Error (ErrorResponse(..), ErrorType)
import           JMAP.Types.Request (Request)
import           JMAP.Types.Response (Response)
import           JMAP.Types.Session (Account, Session)
import           Refined
import           Test.QuickCheck
import           Test.QuickCheck.Instances.UnorderedContainers ()

instance Arbitrary Account where
  arbitrary = R.genericArbitraryU
  shrink = genericShrink

instance Arbitrary ErrorResponse where
  arbitrary = ErrorResponse <$> R.genericArbitraryU
  shrink (ErrorResponse e) = ErrorResponse <$> genericShrink e

instance Arbitrary ErrorType where
  arbitrary = R.genericArbitraryU
  shrink = genericShrink

instance Arbitrary Id where
  arbitrary = Id <$> safeCharVec `suchThatMap` (maybeRefine . T.pack)
    where safeCharVec = chooseInt (1,255) >>= flip vectorOf safeChar
          safeChar = arbitraryASCIIChar `suchThat` isSafeChar
          isSafeChar c = c == '-' || c == '_' || C.isAlphaNum c
          maybeRefine = rightToMaybe . refine
  shrink = genericShrink

instance Arbitrary Invocation where
  arbitrary = R.genericArbitraryU
  shrink = genericShrink

instance Arbitrary Request where
  arbitrary = R.genericArbitraryU
  shrink = genericShrink

instance Arbitrary Response where
  arbitrary = R.genericArbitraryU
  shrink = genericShrink

instance Arbitrary Session where
  arbitrary = R.genericArbitraryU
  shrink = genericShrink

instance Arbitrary UInt where
  arbitrary = UInt <$> chooseInteger bs `suchThatMap` imRefine
    where bs = (fromUInt minBound, fromUInt maxBound)
          imRefine = rightToMaybe . refine . fromInteger
  shrink = genericShrink

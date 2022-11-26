{-# OPTIONS_GHC -Wno-orphans #-}

module JMAP.Types.Base.Arbitrary () where

import qualified Data.Char as C
import qualified Data.Text as T
import           Data.Text.Arbitrary ()
import           Generic.Random
import           GHC.Data.Maybe (rightToMaybe)
import           JMAP.Types.Base
import           Refined
import           Test.QuickCheck

instance Arbitrary Id where
  arbitrary = Id <$> safeCharVec `suchThatMap` (maybeRefine . T.pack)
    where safeCharVec = chooseInt (1,255) >>= flip vectorOf safeChar
          safeChar = arbitraryASCIIChar `suchThat` isSafeChar
          isSafeChar c = c == '-' || c == '_' || C.isAlphaNum c
          maybeRefine = rightToMaybe . refine
  shrink = genericShrink

instance Arbitrary UInt where
  arbitrary = UInt <$> chooseInteger bs `suchThatMap` imRefine
    where bs = (fromUInt minBound, fromUInt maxBound)
          imRefine = rightToMaybe . refine . fromInteger
  shrink = genericShrink

instance Arbitrary Invocation where
  arbitrary = genericArbitraryU
  shrink = genericShrink

{-# OPTIONS_GHC -Wno-orphans #-}

module JMAP.Server.CoreCapability.Arbitrary () where

import           Data.Text.Arbitrary ()
import           Generic.Random
import           JMAP.Server.CoreCapability
import           JMAP.Types.Base.Arbitrary ()
import           Test.QuickCheck

instance Arbitrary CoreCapability where
  arbitrary = genericArbitraryU
  shrink = genericShrink

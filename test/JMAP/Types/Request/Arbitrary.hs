{-# OPTIONS_GHC -Wno-orphans #-}

module JMAP.Types.Request.Arbitrary () where

import           Data.Text.Arbitrary ()
import           Generic.Random
import           JMAP.Types.Request
import           JMAP.Types.Base.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances.UnorderedContainers ()

instance Arbitrary Request where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Invocation where
  arbitrary = genericArbitraryU
  shrink = genericShrink

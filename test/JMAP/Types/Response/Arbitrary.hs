{-# OPTIONS_GHC -Wno-orphans #-}

module JMAP.Types.Response.Arbitrary () where

import           Data.Text.Arbitrary ()
import           Generic.Random
import           JMAP.Types.Response
import           JMAP.Types.Base.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances.UnorderedContainers ()

instance Arbitrary Response where
  arbitrary = genericArbitraryU
  shrink = genericShrink

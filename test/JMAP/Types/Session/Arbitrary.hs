{-# OPTIONS_GHC -Wno-orphans #-}

module JMAP.Types.Session.Arbitrary () where

import           Data.Text.Arbitrary ()
import           Generic.Random
import           JMAP.Types.Session
import           Test.QuickCheck
import           Test.QuickCheck.Instances.UnorderedContainers ()

instance Arbitrary Session where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Accounts where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary PrimaryAccounts where
  arbitrary = genericArbitraryU
  shrink = genericShrink

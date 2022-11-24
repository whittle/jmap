{-# OPTIONS_GHC -Wno-orphans #-}

module JMAP.Types.Base.Arbitrary () where

import           GHC.Data.Maybe (rightToMaybe)
import           JMAP.Types.Base
import           Refined
import           Test.QuickCheck

instance Arbitrary UInt where
  arbitrary = UInt <$> chooseInteger bs `suchThatMap` imRefine
    where bs = (fromUInt minBound, fromUInt maxBound)
          imRefine = rightToMaybe . refine . fromInteger
  shrink = genericShrink

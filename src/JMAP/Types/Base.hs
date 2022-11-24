{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module JMAP.Types.Base
  ( UInt(..)
  , fromUInt
  , Collation
  , URI
  ) where

import qualified Data.Aeson as A
import qualified Data.Maybe as U
import           Data.Text (Text)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.TypeLits
import           Refined

-- TODO: Smuggle this into types
maxNum :: SomeNat
maxNum = U.fromJust $ someNatVal $ (2^53) - 1

-- | The "Unsigned Int" of Section 1.3. The value MUST be in the range
-- 0 <= value <= 2^53-1.
newtype UInt = UInt (Refined (To 9007199254740989) Word64)
  deriving (Eq, Generic, Show)

instance Bounded UInt where
  minBound = UInt $$(refineTH 0)
  maxBound = UInt $$(refineTH 9007199254740989)

instance A.FromJSON UInt

instance A.ToJSON UInt where
  toEncoding = A.genericToEncoding A.defaultOptions

fromUInt :: Num b => UInt -> b
fromUInt (UInt r) = fromIntegral $ unrefine r


-- | RFC 4790
type Collation = Text


-- | Placeholder for when I need to do some actual processing on URIs.
type URI = Text

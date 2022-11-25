{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module JMAP.Types.Base
  ( Id(..)
  , fromId
  , UInt(..)
  , fromUInt
  , Collation
  , URI
  ) where

import qualified Data.Aeson as A
import qualified Data.Char as C
import qualified Data.Hashable as H
import qualified Data.Maybe as U
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (typeRep)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.TypeLits
import           Refined

-- | Predicate used by @Id@.
data SafeChars = SafeChars
  deriving (Generic)

instance Predicate SafeChars Text where
  validate p x = if T.all isSafeChar x
                 then Nothing
                 else throwRefineOtherException
                      (typeRep p)
                      "Text contains unsafe characters [RFC 8620 § 1.2]."

isSafeChar :: Char -> Bool
isSafeChar c = c == '-' || c == '_' || (C.isAscii c && C.isAlphaNum c)


-- | The Id type defined in Section 1.2. All record ids are assigned
-- by the server and are immutable.
--
-- Where "Id" is given as a data type, it means a "String" of at least
-- 1 and a maximum of 255 octets in size, and it MUST only contain
-- characters from the "URL and Filename Safe" base64 alphabet, as
-- defined in Section 5 of [RFC4648], excluding the pad character
-- ("="). This means the allowed characters are the ASCII alphanumeric
-- characters ("A-Za-z0-9"), hyphen ("-"), and underscore ("_").
--
-- These characters are safe to use in almost any context (e.g.,
-- filesystems, URIs, and IMAP atoms). For maximum safety, servers
-- SHOULD also follow defensive allocation strategies to avoid
-- creating risks where glob completion or data type detection may be
-- present (e.g., on filesystems or in spreadsheets). In particular,
-- it is wise to avoid:
--
-- * Ids starting with a dash
-- * Ids starting with digits
-- * Ids that contain only digits
-- * Ids that differ only by ASCII case (for example, A vs. a)
-- * the specific sequence of three characters "NIL" (because this
--   sequence can be confused with the IMAP protocol expression of the
--   null value)
newtype Id = Id (Refined (NonEmpty && SizeLessThan 256 && SafeChars) Text)
  deriving (Eq, A.FromJSONKey, Generic, Show, A.ToJSONKey)

instance A.FromJSON Id

instance H.Hashable Id

instance A.ToJSON Id where
  toEncoding = A.genericToEncoding A.defaultOptions

fromId :: Id -> Text
fromId (Id t) = unrefine t


-- TODO: Smuggle this into types
maxNum :: SomeNat
maxNum = U.fromJust $ someNatVal $ (2^53) - 1

-- | The "Unsigned Int" defined in Section 1.3. The value MUST be in
-- the range 0 <= value <= 2^53-1.
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

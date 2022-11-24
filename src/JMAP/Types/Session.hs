{-# LANGUAGE DeriveGeneric #-}

module JMAP.Types.Session
  ( Session(..)
  , Capabilities(..)
  , Accounts(..)
  , PrimaryAccounts(..)
  ) where

import           Data.Aeson
import           Data.Text (Text)
import           GHC.Generics (Generic)

data Session = Session
  { capabilities :: !Capabilities
  , accounts :: !Accounts
  , primaryAccounts :: !PrimaryAccounts
  , username :: !(Maybe Text)
  , apiUrl :: !(Maybe Text)
  , downloadUrl :: !(Maybe Text)
  , uploadUrl :: !(Maybe Text)
  , eventSourceUrl :: !(Maybe Text)
  , state :: !(Maybe Text)
  } deriving (Eq, Generic, Show)

instance FromJSON Session

instance ToJSON Session where
  toEncoding = genericToEncoding defaultOptions


data Capabilities = Capabilities
  deriving (Eq, Generic, Show)

instance FromJSON Capabilities

instance ToJSON Capabilities where
  toEncoding = genericToEncoding defaultOptions


data Accounts = Accounts
  deriving (Eq, Generic, Show)

instance FromJSON Accounts

instance ToJSON Accounts where
  toEncoding = genericToEncoding defaultOptions


data PrimaryAccounts = PrimaryAccounts
  deriving (Eq, Generic, Show)

instance FromJSON PrimaryAccounts

instance ToJSON PrimaryAccounts where
  toEncoding = genericToEncoding defaultOptions

{-# LANGUAGE DeriveGeneric #-}

module JMAP.Types.Session
  ( Session(..)
  , Capabilities
  , Accounts
  , Account(..)
  ) where

import qualified Data.Aeson as A
import           Data.HashMap.Strict (HashMap)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           JMAP.Types.Base

-- | The JMAP Session Resource of Section 2. A successful
-- authenticated GET request to the JMAP Session resource MUST return
-- a JSON-encoded Session object, giving details about the data and
-- capabilities the server can provide to the client given those
-- credentials.  It has the following properties:
data Session = Session
  { capabilities :: !Capabilities
    -- ^ An object specifying the capabilities of this server. Each
    -- key is a URI for a capability supported by the server. The
    -- value for each of these keys is an object with further
    -- information about the server's capabilities in relation to that
    -- capability. The capabilities object MUST include a property
    -- called "urn:ietf:params:jmap:core".
  , accounts :: !Accounts
    -- ^ A map of an account id to an Account object for each account
    -- (see Section 1.6.2) the user has access to.
  , primaryAccounts :: !(HashMap URI Id)
    -- ^ A map of capability URIs (as found in accountCapabilities) to
    -- the account id that is considered to be the user's main or
    -- default account for data pertaining to that capability. If no
    -- account being returned belongs to the user, or in any other way
    -- there is no appropriate way to determine a default account,
    -- there MAY be no entry for a particular URI, even though that
    -- capability is supported by the server (and in the capabilities
    -- object). "urn:ietf:params:jmap:core" SHOULD NOT be present.
  , username :: !(Maybe Text)
    -- ^ The username associated with the given credentials, or the empty string if none.
  , apiUrl :: !(Maybe Text)
    -- ^ The URL to use for JMAP API requests.
  , downloadUrl :: !(Maybe Text)
    -- ^ The URL endpoint to use when downloading files, in URI
    -- Template (level 1) format [RFC6570]. The URL MUST contain
    -- variables called "accountId", "blobId", "type", and "name". The
    -- use of these variables is described in Section 6.2. Due to
    -- potential encoding issues with slashes in content types, it is
    -- RECOMMENDED to put the "type" variable in the query section of
    -- the URL.
  , uploadUrl :: !(Maybe Text)
    -- ^ The URL endpoint to use when uploading files, in URI Template
    -- (level 1) format [RFC6570]. The URL MUST contain a variable
    -- called "accountId". The use of this variable is described in
    -- Section 6.1.
  , eventSourceUrl :: !(Maybe Text)
    -- ^ The URL to connect to for push events, as described in
    -- Section 7.3, in URI Template (level 1) format [RFC6570]. The
    -- URL MUST contain variables called "types", "closeafter", and
    -- "ping". The use of these variables is described in Section 7.3.
  , state :: !(Maybe Text)
    -- ^ A (preferably short) string representing the state of this
    -- object on the server. If the value of any other property on the
    -- Session object changes, this string will change. The current
    -- value is also returned on the API Response object (see Section
    -- 3.4), allowing clients to quickly determine if the session
    -- information has changed (e.g., an account has been added or
    -- removed), so they need to refetch the object.
  } deriving (Eq, Generic, Show)

instance A.FromJSON Session

instance A.ToJSON Session where
  toEncoding = A.genericToEncoding A.defaultOptions


-- | Section 2 gives the type as "String[Object]". Specifications for
-- future capabilities will define their own properties on the
-- capabilities object. Servers MAY advertise vendor-specific JMAP
-- extensions, as described in Section 1.8. To avoid conflict, an
-- identifier for a vendor-specific extension MUST be a URL with a
-- domain owned by the vendor. Clients MUST opt in to any capability
-- it wishes to use (see Section 3.3).
type Capabilities = HashMap URI A.Value


-- | A map of an account id to an Account object for each account (see
-- Section 1.6.2) the user has access to.
type Accounts = HashMap Id Account


data Account = Account
  { name :: !Text
    -- ^ A user-friendly string to show when presenting content from
    -- this account, e.g., the email address representing the owner of
    -- the account.
  , isPersonal :: !Bool
    -- ^ This is true if the account belongs to the authenticated user
    -- rather than a group account or a personal account of another
    -- user that has been shared with them.
  , isReadOnly :: !Bool
    -- ^ This is true if the entire account is read-only.
  , accountCapabilities :: !Capabilities
    -- ^ The set of capability URIs for the methods supported in this
    -- account. Each key is a URI for a capability that has methods
    -- you can use with this account. The value for each of these keys
    -- is an object with further information about the account's
    -- permissions and restrictions with respect to this capability,
    -- as defined in the capability's specification.
    --
    -- The client MUST ignore any properties it does not
    -- understand. The server advertises the full list of capabilities
    -- it supports in the capabilities object, as defined above. If
    -- the capability defines new methods, the server MUST include it
    -- in the accountCapabilities object if the user may use those
    -- methods with this account. It MUST NOT include it in the
    -- accountCapabilities object if the user cannot use those methods
    -- with this account.
    --
    -- For example, you may have access to your own account with mail,
    -- calendars, and contacts data and also a shared account that
    -- only has contacts data (a business address book, for
    -- example). In this case, the accountCapabilities property on the
    -- first account would include something like
    -- "urn:ietf:params:jmap:mail", "urn:ietf:params:jmap:calendars",
    -- and "urn:ietf:params:jmap:contacts", while the second account
    -- would just have the last of these.
    --
    -- Attempts to use the methods defined in a capability with one of
    -- the accounts that does not support that capability are rejected
    -- with an "accountNotSupportedByMethod" error (see "Method-Level
    -- Errors", Section 3.6.2).
  } deriving (Eq, Generic, Show)

instance A.FromJSON Account

instance A.ToJSON Account where
  toEncoding = A.genericToEncoding A.defaultOptions

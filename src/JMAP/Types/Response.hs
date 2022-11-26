{-# LANGUAGE DeriveGeneric #-}

module JMAP.Types.Response
  ( Response(..)
  ) where

import qualified Data.Aeson as A
import           Data.HashMap.Strict (HashMap)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           JMAP.Types.Base

-- | The Response object as described in Section 3.4.
data Response = Response
  { methodResponses :: ![Invocation]
    -- ^ An array of responses, in the same format as the
    -- "methodCalls" on the Request object. The output of the methods
    -- MUST be added to the "methodResponses" array in the same order
    -- that the methods are processed.
    --
    -- Unless otherwise specified, if the method call completed
    -- successfully, its response name is the same as the method name
    -- in the request.
  , createdIds :: !(Maybe (HashMap Id Id))
    -- ^ A map of a (client-specified) creation id to the id the
    -- server assigned when a record was successfully created. This
    -- MUST include all creation ids passed in the original createdIds
    -- parameter of the Request object, as well as any additional ones
    -- added for newly created records.
  , sessionState :: !Text
    -- ^ The current value of the "state" string on the Session
    -- object, as described in Section 2. Clients may use this to
    -- detect if this object has changed and needs to be refetched.
  } deriving (Eq, Generic, Show)

instance A.FromJSON Response

instance A.ToJSON Response where
  toEncoding = A.genericToEncoding A.defaultOptions

{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}

module Network.JMAP
  ( getSessionJSON
  , getSessionValue
  , getMailAccountId
  , getMailAccountId'
  , getApiUrl
  , getApiUrl'
  ) where

import Control.Lens ((&), (^.), (^?), (.~), (?~))
import Data.Aeson (FromJSON, Value)
import Data.Aeson.Lens (key, _String)
import Data.Map (Map, lookup)
import Data.Text (Text)
import Data.ByteString
import GHC.Generics (Generic)
import qualified Control.Exception as E

import Network.Wreq

--
-- Constants
--
vacationRespURN::Text
submissionURN::Text
mailURN::Text
urlWellknown::String

vacationRespURN = "urn:ietf:params:jmap:vacationresponse"
submissionURN = "urn:ietf:params:jmap:submission"
mailURN = "urn:ietf:params:jmap:mail"

urlWellknown = "https://jmap.fastmail.com/.well-known/jmap"

data GetBody = GetBody {
  primaryAccounts :: Map Text Text
  , state :: Text
  , apiUrl :: Text
  , eventSourceUrl :: Text
  , downloadUrl :: Text
  , uploadUrl :: Text
  , username :: Text
  } deriving (Show, Generic)

instance FromJSON GetBody

--
-- JSON interfaces
--
getSessionJSON :: ByteString-> ByteString -> IO (Response GetBody)
getSessionJSON u p = asJSON =<< getWith opts urlWellknown
  where
    opts = defaults & auth ?~ basicAuth u p

-- Get accountId from a JSON response body
getMailAccountId :: (Response GetBody) -> Maybe Text
getMailAccountId r = Data.Map.lookup (mailURN::Text) $ (primaryAccounts (r ^. responseBody))

-- Get apiUrl
getApiUrl :: (Response GetBody) -> Text --TODO: Convert to Maybe
getApiUrl r = (apiUrl (r ^. responseBody))
--
-- Value interfaces
--
getSessionValue :: ByteString -> ByteString -> IO (Response Value)
getSessionValue u p = asValue =<< getWith opts urlWellknown
  where
    opts = defaults & auth ?~ basicAuth u p

-- Get accountId from a value response body
getMailAccountId' :: (Response Value) -> Text
getMailAccountId' r = r ^. responseBody . key "primaryAccounts" . key mailURN . _String

-- Get apiUrl
getApiUrl' :: (Response Value) -> Text
getApiUrl' r = r ^. responseBody . key "apiUrl" . _String


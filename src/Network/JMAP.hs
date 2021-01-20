{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}

module Network.JMAP
  (jmapAuth
  , mkConnectionData
  , ConnectionType(..)
  , Login(..)
  , Session(..)
  , User(..)
  , renderLoginError
  ) where

import Control.Lens ((&), (^.), (^?), (.~), (?~))
import Data.Aeson (FromJSON, Value)
import Data.Aeson.Lens (key, _String)
import Data.Map (Map, lookup)
import Data.Text (Text, unpack)
import Data.ByteString
import GHC.Generics (Generic)
import qualified Control.Exception as E
import qualified Data.Text.Encoding as TE

import Network.Wreq

import Network.JMAP.Exceptions
import Network.JMAP.Util
import Network.JMAP.Types.Base
import Network.JMAP.Types.Internal
import Network.JMAP.Types


--
-- Constants
--
vacationRespURN::Text
submissionURN::Text
mailURN::Text

vacationRespURN = "urn:ietf:params:jmap:vacationresponse"
submissionURN = "urn:ietf:params:jmap:submission"
mailURN = "urn:ietf:params:jmap:mail"

-- Authenticate to server
jmapAuth :: ConnectionData -> Login -> IO (Either LoginFailureException Session)
jmapAuth cd login = do
  let lurl = authURL cd
      opts = defaults & auth ?~ basicAuth (TE.encodeUtf8 $ username login) (TE.encodeUtf8 $ password login)
  r <- asValue =<< getWith opts (Data.Text.unpack $ serverURLtoText lurl)
  let sCode = r ^. responseStatus . statusCode
  if (sCode /= 200)
    then do
        let eMsg = "Server returned unexpected " <> (show sCode) <> " response."
        return $ Left $ LoginFailureException eMsg
    else do
        let maId = r ^. responseBody . key "primaryAccounts" . key mailURN . _String
            saId = r ^. responseBody . key "primaryAccounts" . key submissionURN . _String
            dUrl = r ^. responseBody . key "downloadUrl" . _String
            uUrl = r ^. responseBody . key "uploadUrl" . _String
            apiUrl = r ^. responseBody . key "apiUrl" . _String
            eUrl = r ^. responseBody . key "eventSourceUrl" . _String
        return (Right $ (Session cd (User (username login) maId saId dUrl uUrl apiUrl eUrl)))

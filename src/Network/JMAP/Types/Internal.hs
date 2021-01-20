{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.JMAP.Types.Internal where

import Network.JMAP.Types.Base
import qualified Data.Text as T


urlWellKnown::T.Text
urlWellKnown = "/.well-known/jmap"

data ConnectionType =
    ConnectHTTPS Bool
  | ConnectHTTP
  deriving (Eq, Show, Read)

data ConnectionData = ConnectionData {
    cdHostUrl          :: Hostname
  , cdPort             :: Port
  , cdConnectionType   :: ConnectionType
  } deriving (Eq, Show, Read)

newtype ServerBaseURL = ServerBaseURL T.Text
                      deriving (Eq, Show)

authURL :: ConnectionData -> ServerBaseURL
authURL cd =
  let scheme = case cdConnectionType cd of
        ConnectHTTPS {} -> "https"
        ConnectHTTP  {} -> "http"
      host = cdHostUrl cd
      port = T.pack $
             if cdConnectionType cd == ConnectHTTP
             then if cdPort cd == 80 then "" else ":" <> show (cdPort cd)
             else if cdPort cd == 443 then "" else ":" <> show (cdPort cd)
      path1 = urlWellKnown
      path2 = if "/" `T.isPrefixOf` path1
              then path1 else "/" <> path1
  in ServerBaseURL $ scheme <> "://" <> host <> port <> path2

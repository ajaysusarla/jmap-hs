module Network.JMAP.Types
  ( module Network.JMAP.Types
  , module Network.JMAP.Types.Base
  , ConnectionType(..)
  , authURL
  , ServerBaseURL(..)
  ) where


import           Data.Text (Text)

import           Network.JMAP.Types.Base
import           Network.JMAP.Types.Internal

data Login = Login
  {
    username :: Text
  , password :: Text
  }

data Session = Session
  {
    conn  :: ConnectionData
  , uinfo :: User
  } deriving (Show)

data User = User
  {
    uname :: Text
  , mailAccountId :: Text
  , submissionAccoundId :: Text
  , downloadURL :: Text
  , uploadURL :: Text
  , apiURL :: Text
  , eventSourceURL :: Text
  } deriving (Show)

serverURLtoText :: ServerBaseURL -> Text
serverURLtoText (ServerBaseURL u) = u

mkConnectionData :: Hostname -> Port -> ConnectionType -> ConnectionData
mkConnectionData host port connType = ConnectionData
  { cdHostUrl        = host
  , cdPort           = port
  , cdConnectionType = connType
  }

initConnectionData :: Hostname -> Port -> ConnectionType -> IO ConnectionData
initConnectionData host port connType = do
  return (mkConnectionData host port connType)

destroyConnectionData :: ConnectionData -> ()
destroyConnectionData cd = ()


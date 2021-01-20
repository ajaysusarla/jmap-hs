{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.JMAP.Exceptions
  (
    LoginFailureException(..)
  , renderLoginError
  ) where

import qualified Data.Aeson as A
import qualified Data.Text as T
import           Data.Typeable ( Typeable )
import           Control.Exception ( Exception(..) )


data LoginFailureException = LoginFailureException String
  deriving (Show, Typeable)

instance Exception LoginFailureException

renderLoginError :: LoginFailureException -> String
renderLoginError (LoginFailureException msg) = msg

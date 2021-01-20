{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.JMAP.Util
  ((~=)
  ) where

import           Data.Char (toUpper)
import           Control.Exception(Exception, throwIO)

import           Network.JMAP.Types.Base

-- | Case Insensitive string comparison
(~=) :: String -> String -> Bool
a ~= b = map toUpper a == map toUpper b


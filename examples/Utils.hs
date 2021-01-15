module Utils
  ( getOptions
  ,  getCreds
  ) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Console.GetOpt
import Control.Monad (when)
import qualified Data.Text as T

data Options = Options
  { optUserName :: T.Text
  , optPassword :: T.Text
  } deriving (Eq, Read, Show)

defaultOptions :: Options
defaultOptions = Options
  { optUserName = T.empty
  , optPassword = T.empty
  }

optDesc :: [OptDescr (Options -> IO Options)]
optDesc =
  [ Option "u" ["user"]
    (ReqArg (\arg opt -> return opt { optUserName = T.pack $ arg })
    "Username")
    "The username"
  , Option "p" ["pass"]
    (ReqArg (\arg opt -> return opt { optPassword = T.pack $ arg })
    "Password")
    "The password"
  ]

getOptions :: IO Options
getOptions = do
  args <- getArgs
  case getOpt RequireOrder optDesc args of
    (actions, [], []) -> do
      foldl (>>=) (return defaultOptions) actions
    (_, _, errors) -> do
      mapM_ putStr errors
      exitFailure

getCreds :: Options -> IO (T.Text, T.Text)
getCreds opts = do
  let uname = optUserName opts
  when (T.null uname) $ do
    putStrLn "Need a username"
    exitFailure

  let pass = optPassword opts
  when (T.null pass) $ do
    putStrLn "Need a password"
    exitFailure

  return (uname, pass)


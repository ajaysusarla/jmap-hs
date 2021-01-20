{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.JMAP
import qualified Data.Text.Encoding as TE

import Utils

main :: IO ()
main = do
  opts <- getOptions
  (user, pass) <- getCreds opts
  let login = Login user pass
      cd = mkConnectionData "jmap.fastmail.com" 443 (ConnectHTTPS True)
  result <- jmapAuth cd login
  case result of
    Left e -> do
      putStrLn "Authentication Error:::"
      print (renderLoginError e)
    Right s -> do
      let u = uinfo s
          aId = mailAccountId u
          aURL = apiURL u
      putStrLn $ "AccountId : " ++ show aId
      putStrLn $ "API URL   : " ++ show aURL


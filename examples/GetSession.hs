{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.JMAP
import Text.Show.Pretty (pPrint)
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
      pPrint s

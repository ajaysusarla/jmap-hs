#!/usr/bin/env stack
-- stack --resolver lts-16.28 runghc --package jmap-hs

{-# LANGUAGE OverloadedStrings #-}

import Network.JMAP
import qualified Data.Text.Encoding as TE

import Utils

main :: IO ()
main = do
  opts <- getOptions
  (uname, pass) <- getCreds opts

  s <- getSessionValue (TE.encodeUtf8 uname) (TE.encodeUtf8 pass)

  let aId = getMailAccountId' s
      apiUrl = getApiUrl' s
  putStrLn $ "AccountId : " ++ show aId
  putStrLn $ "API URL   : " ++ show apiUrl

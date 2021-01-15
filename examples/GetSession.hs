#!/usr/bin/env stack
-- stack --resolver lts-16.28 runghc --package jmap-hs

{-# LANGUAGE OverloadedStrings #-}

import Network.JMAP
import Text.Show.Pretty (pPrint)
import qualified Data.Text.Encoding as TE

import Utils

main :: IO ()
main = do
  opts <- getOptions
  (uname, pass) <- getCreds opts

  s <- getSessionValue (TE.encodeUtf8 uname) (TE.encodeUtf8 pass)
  pPrint s

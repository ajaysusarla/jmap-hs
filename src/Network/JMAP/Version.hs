{-# LANGUAGE TemplateHaskell #-}

module Network.JMAP.Version (jmapApiVersion) where

import Data.Version (showVersion)
import Development.GitRev (gitBranch, gitHash)
import Paths_jmap_hs (version)

jmapApiVersion :: String
jmapApiVersion
  | $(gitHash) == ("UNKNOWN" :: String) = "jmap-api-hs " ++ showVersion version
  | otherwise = "jmap-api-hs " ++ showVersion version ++ " (" ++ $(gitBranch) ++ "@" ++ take 7 $(gitHash) ++ ")"


{-# LANGUAGE DeriveGeneric #-}
module State where

import Data.Maybe (fromMaybe)
import Control.Exception (throwIO)

import Data.ByteString (readFile, writeFile)
import Prelude hiding (readFile, writeFile)
import Data.Time.Clock
import GHC.Generics
import Data.Yaml as YAML
import           System.Directory    (getHomeDirectory, doesFileExist)
import           System.FilePath     ((</>))

import JIRA as J
import Core

type Log = [LogLine]

data LogLine = LogLine
  { activeIssueKey :: J.IssueKey
  , started        :: UTCTime
  } deriving (Eq, Show, Generic)

instance FromJSON LogLine
instance ToJSON LogLine

loadState :: Maybe FilePath -> IO Log
loadState path = do
  defaultPath <- (</> ".jira-cli/log.yaml") <$> getHomeDirectory
  exists <- doesFileExist defaultPath
  if exists
    then do txt <- readFile (fromMaybe defaultPath path)
            maybe (throwIO $ ParseException txt) return (YAML.decode txt)
    else return []

writeState :: Maybe FilePath -> Log -> IO ()
writeState path state = do
  defaultPath <- (</> ".jira-cli/log.yaml") <$> getHomeDirectory
  writeFile (fromMaybe defaultPath path) (YAML.encode state)

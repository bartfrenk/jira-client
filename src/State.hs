{-# LANGUAGE DeriveGeneric #-}
module State where

import           Control.Exception   (throwIO)
import           Data.ByteString     (readFile, writeFile)
import           Data.Maybe          (fromMaybe)
import           Data.Time.Clock     (diffUTCTime)
import           Data.Time.LocalTime
import           Data.Yaml           as YAML
import           GHC.Generics
import           Prelude             hiding (log, readFile, writeFile)
import           System.Directory    (doesFileExist, getHomeDirectory)
import           System.FilePath     ((</>))

import           Core
import qualified JIRA                as J
import Concepts

type Log = [LogLine]

data LogLineType = Started J.IssueKey | Stopped
  deriving (Eq, Show, Generic)

instance FromJSON LogLineType
instance ToJSON LogLineType

data LogLine = LogLine ZonedTime LogLineType
  deriving (Show, Generic)

diffToSeconds :: ZonedTime -> ZonedTime -> Integer
diffToSeconds t s =
  let delta = diffUTCTime (zonedTimeToUTC t) (zonedTimeToUTC s)
  in truncate (realToFrac delta :: Double)

toWorkLog :: Log -> ([J.WorkLog], Log)
toWorkLog (LogLine s (Started key) : end@(LogLine t _) : rest) =
  let (wl, log) = toWorkLog (end:rest)
      sec = diffToSeconds t s
  in (WorkLog key (fromSeconds sec) s : wl, log)
toWorkLog (_:rest) = toWorkLog rest
toWorkLog [] = ([], [])

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

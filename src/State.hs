{-# LANGUAGE DeriveGeneric #-}
module State where

import           Control.Exception   (throwIO)
import           Data.ByteString     (readFile, writeFile)
import           Data.Maybe          (fromMaybe)
import           Data.Time.LocalTime
import           Data.Time.Clock      (diffUTCTime)
import           Data.Yaml           as YAML
import           GHC.Generics
import           Prelude             hiding (log, readFile, writeFile)
import           System.Directory    (doesFileExist, getHomeDirectory)
import           System.FilePath     ((</>))

import           Concepts
import           Core
import           Orphans()

type Log = [LogLine]

data LogLineType = Started IssueKey | Stopped
  deriving (Eq, Show, Generic)

instance FromJSON LogLineType
instance ToJSON LogLineType

data LogLine = LogLine ZonedTime LogLineType
  deriving (Show, Generic, Eq)

isStarted :: LogLine -> Bool
isStarted (LogLine _ (Started _)) = True
isStarted _ = False

instance Ord LogLine where
  (LogLine s _) `compare` (LogLine t _) = s `compare` t

diffToSeconds :: ZonedTime -> ZonedTime -> Integer
diffToSeconds t s =
  let delta = diffUTCTime (zonedTimeToUTC t) (zonedTimeToUTC s)
  in truncate (realToFrac delta :: Double)

-- TODO: combine with nextWorkLogItem
toWorkLog :: Log -> ([WorkLog], Log)
toWorkLog (LogLine s (Started key) : end@(LogLine t _) : rest) =
  let (wl, log) = toWorkLog (end:rest)
      spent = computeTimeSpent s t
  in (WorkLog key spent s : wl, log)
toWorkLog log@[LogLine _ (Started _)] = ([], log)
toWorkLog [] = ([], [])
toWorkLog (_:rest) = toWorkLog rest

nextWorkLogItem :: Log -> (Maybe WorkLog, Log)
nextWorkLogItem (LogLine start (Started key) : end@(LogLine finish ty) : rest) =
  let spent = computeTimeSpent start finish
  in case ty of
    Stopped   -> (Just $ WorkLog key spent start, rest)
    Started _ -> (Just $ WorkLog key spent start, end:rest)
nextWorkLogItem log@[LogLine _ (Started _)] = (Nothing, log)
nextWorkLogItem (_:rest) = nextWorkLogItem rest
nextWorkLogItem [] = (Nothing, [])

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

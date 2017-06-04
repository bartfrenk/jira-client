{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Run where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Conduit
import qualified Data.Conduit.List    as CL
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromMaybe, isNothing)
import           Data.Semigroup       ((<>))
import           Data.String.Conv     (toS)
import qualified Data.Text            as T
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.LocalTime
import           Prelude              hiding (log)

import           Concepts
import qualified Format               as F
import qualified JIRA                 as J
import           Options
import           State

type Failure = String

-- TODO: better to make this a newtype to hide the implementation
type CommandM = ExceptT Failure (ReaderT Options (StateT Log IO))

runCommandM :: CommandM a -> Options -> Log -> IO (Either Failure a, Log)
runCommandM act opts =
  runStateT (runReaderT (runExceptT act) opts)
runWithOptions :: J.EnvM a -> CommandM a
runWithOptions act = liftIO . J.runEnv act . makeEnv =<< ask

log :: J.IssueKey -> J.TimeSpent -> CommandM (Maybe String)
log issueKey timeSpent = do
  now <- liftIO getZonedTime
  void $ runWithOptions (act now)
  return $ Just $ "Logged " <> toS (toDurationString timeSpent)
  where act t = J.log $ J.WorkLog issueKey timeSpent t

search :: J.JQL -> CommandM (Maybe String)
search jql = do
  jql' <- lookupQuery jql
  runWithOptions $ runConduit (J.issueSearch jql' $= printer)
  return Nothing

lookupQuery :: MonadReader Options m => J.JQL -> m J.JQL
lookupQuery orig@(J.JQL t) =
  case T.split (== ':') t of
    ["jql", key] -> fromMaybe orig . Map.lookup key <$> reader queries
    _            -> return orig

printer :: MonadIO m => Sink J.Issue m ()
printer = CL.mapM_ (liftIO . putStrLn . toS . J.formatIssue)


getActiveIssue :: (MonadState Log m) => m (Maybe J.IssueKey)
getActiveIssue = toIssueKey . last <$> get
  where toIssueKey (LogLine _ (Started issueKey)) = Just issueKey
        toIssueKey _                              = Nothing

-- TODO: allow comments, or
-- maybe categories of work: i.e. code-review, development
-- TODO: nicer validation
start :: J.IssueKey -> CommandM (Maybe String)
start issueKey = do
  exists <- runWithOptions (J.issueExists issueKey)
  if exists then do
    active <- getActiveIssue
    if isNothing active || active /= Just issueKey then do
      now <- liftIO getZonedTime
      modify (<> [LogLine now (Started issueKey)])
      return $ Just $ "Started working on " <> toS (toText issueKey)
    else return $ Just $ "Already working on " <> toS (toText issueKey)
  else throwError (toS (toText issueKey) <> " does not exists")

stop :: CommandM (Maybe String)
stop = do
  active <- getActiveIssue
  now <- liftIO getZonedTime
  case active of
    Just issueKey -> do
      modify (<> [LogLine now Stopped])
      return $ Just $ "Stopped working on " <> toS (toText issueKey)
    Nothing ->
      return $ Just "No active issue"

-- TODO: Use pretty printing library for better overview
--  - Less detailed start time
--  - Show gaps
--  - Show summary per day
--  - refactor: split out in 'putActiveIssue' and 'putWorkLog'
review :: CommandM (Maybe String)
review = do
  active <- getActiveIssue
  (workLog, _) <- toWorkLog <$> get
  F.putDoc $
    activeDoc active F.</$>
    F.hardline F.<>
    workLogDoc (removeInvalid workLog) F.</$>
    F.hardline
  return Nothing
  where workLogDoc [] = F.text "Work log is empty"
        workLogDoc workLog =
          F.text "Work log to commit" F.</$>
          F.indent 4 (F.format workLog)
        activeDoc Nothing = F.text "No active issue"
        activeDoc (Just issueKey) =
          F.text "Active issue" F.</$>
          F.hardline F.<>
          F.indent 4 (F.format issueKey)
        removeInvalid =
          filter ((>= 60) . toSeconds . timeSpent)

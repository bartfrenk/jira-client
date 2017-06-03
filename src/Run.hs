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
import           Data.Maybe           (fromMaybe, maybe, isNothing)
import           Data.Semigroup       ((<>))
import           Data.String.Conv     (toS)
import qualified Data.Text            as T
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.LocalTime
import           Prelude              hiding (log)

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
  now <- liftIO getCurrentTime
  void $ runWithOptions (act now)
  return $ Just $ logResult timeSpent
  where act t = J.log issueKey $ J.WorkLog timeSpent t
        logResult (J.TimeSpentCode txt) =
          "Logged " <> toS txt <> " on " <> J.formatIssueKey issueKey
        logResult (J.TimeSpentSeconds sec) =
          "Logged " <> show sec <> " seconds on " <> J.formatIssueKey issueKey


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
        toIssueKey _ = Nothing

-- TODO: check whether we are already working on requested issue Would be better
-- to have good equality on IssueKey, for example by representing it as a
-- product of normalized project name, and a number.
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
      return $ Just $ "Started working on " <> J.formatIssueKey issueKey
    else return $ Just $ "Already working on " <> J.formatIssueKey issueKey
  else throwError (J.formatIssueKey issueKey <> " does not exists")

stop :: CommandM (Maybe String)
stop = do
  active <- getActiveIssue
  now <- liftIO getZonedTime
  case active of
    Just issueKey -> do
      modify (<> [LogLine now Stopped])
      return $ Just $ "Stopped working on " <> J.formatIssueKey issueKey
    Nothing ->
      return $ Just "Not working on any issue currently"

review :: CommandM (Maybe String)
review = do
  active <- getActiveIssue
  case active of
    Nothing -> liftIO $ putStrLn "No active issue\n"
    Just issueKey -> liftIO $ putStrLn $ "Active issue: "
                                      <> J.formatIssueKey issueKey
                                      <> "\n"
  (workLog, _) <- toWorkLog <$> get
  if not $ null workLog
    then liftIO $ mapM_ displayLine workLog >> return Nothing
    else return $ Just "No items in local work log"
    where
      displayLine (issueKey,  J.WorkLog{..}) = do
        started' <- utcToLocalZonedTime started
        let str = J.formatIssueKey issueKey <> "\t"
              <> displayTime started' <> "\t"
              <> J.displayTimeSpent timeSpent
        liftIO $ putStrLn str

displayTime :: ZonedTime -> String
displayTime = formatTime defaultTimeLocale formatStr
  where formatStr = iso8601DateFormat (Just "%H:%M:%S")

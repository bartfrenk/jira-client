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
import           Data.Maybe           (fromMaybe, maybe)
import           Data.Semigroup       ((<>))
import           Data.String.Conv     (toS)
import           Data.Text            hiding (null)
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
          "Logged " <> toS txt <> " on " <> toS issueKey
        logResult (J.TimeSpentSeconds sec) =
          "Logged " <> show sec <> " seconds on " <> toS issueKey


search :: J.JQL -> CommandM (Maybe String)
search jql = do
  jql' <- lookupQuery jql
  runWithOptions $ runConduit (J.issueSearch jql' $= printer)
  return Nothing

lookupQuery :: MonadReader Options m => J.JQL -> m J.JQL
lookupQuery orig@(J.JQL t) =
  case split (== ':') t of
    ["jql", key] -> fromMaybe orig . Map.lookup key <$> reader queries
    _            -> return orig

printer :: MonadIO m => Sink J.Issue m ()
printer = CL.mapM_ (liftIO . putStrLn . toS . formatIssue)

-- TODO: should go in separate formatter module
-- TODO: should make use of builder for performance
formatIssue :: J.Issue -> Text
formatIssue issue =
     J.key issue <> "\t"
  <> maybe "-" (toS . show) (J.points issue) <> "\t"
  <> J.summary issue

-- TODO: check whether we are already working on requested issue Would be better
-- to have good equality on IssueKey, for example by representing it as a
-- product of normalized project name, and a number.
-- TODO: allow comments, or
-- maybe categories of work: i.e. code-review, development
start :: J.IssueKey -> CommandM (Maybe String)
start issueKey = do
  exists <- runWithOptions (J.issueExists issueKey)
  if exists then do
    now <- liftIO getZonedTime
    modify (<> [LogLine now (Started issueKey)])
    return $ Just $ "Started working on " <> toS issueKey
  else throwError (toS issueKey <> " does not exists")

review :: CommandM (Maybe String)
review = do
  (workLog, _) <- toWorkLog <$> get
  if not $ null workLog
    then liftIO $ mapM_ displayLine workLog >> return Nothing
    else return $ Just "No items in local work log"
    where
      displayLine (issueKey,  J.WorkLog{..}) = do
        started' <- utcToLocalZonedTime started
        let str = toS issueKey <> "\t" <> displayTime started' <> "\t"
              <> J.displayTimeSpent timeSpent
        liftIO $ putStrLn str

displayTime :: ZonedTime -> String
displayTime = formatTime defaultTimeLocale formatStr
  where formatStr = iso8601DateFormat (Just "%H:%M:%S")

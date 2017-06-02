{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

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
import           Data.Text
import           Data.Time.Clock
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
  void $ runWithOptions act
  return $ Just $ logResult timeSpent
  where act = J.log issueKey $ J.WorkLog timeSpent Nothing
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

start :: J.IssueKey -> CommandM (Maybe String)
start issueKey = do
  exists <- runWithOptions (J.issueExists issueKey)
  if exists then do
    now <- liftIO getCurrentTime
    modify (<> [LogLine issueKey now])
    return $ Just $ "Started working on " <> toS issueKey
  else throwError (toS issueKey <> " does not exists")

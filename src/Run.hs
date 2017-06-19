{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Run where

import           Control.Exception.Lifted
import           Control.Monad            ((<=<))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Conduit
import qualified Data.Conduit.List        as CL
import qualified Data.Map.Strict          as M
import           Data.Maybe               (fromMaybe, isNothing)
import           Data.Monoid              (mconcat)
import           Data.Semigroup           ((<>))
import           Data.String.Conv         (toS)
import qualified Data.Text                as T
import           Data.Time.Clock          (addUTCTime)
import           Data.Time.LocalTime
import           Data.Version             (showVersion)
import           Prelude                  hiding (log)

import           Paths_jira_client        (version)

import           Concepts
import qualified Format                   as F
import qualified JIRA                     as J
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

printVersion :: CommandM (Maybe String)
printVersion = liftIO $ putStrLn (showVersion version) >> return Nothing

log :: IssueKey -> TimeSpent -> CommandM (Maybe String)
log key spent = do
  now <- liftIO getZonedTime
  void $ runWithOptions (act now)
  return $ Just $ "Logged " <> toS (toDurationString spent)
  where act t = J.log $ WorkLog key spent t

search :: J.JQL -> CommandM (Maybe String)
search jql = do
  jql' <- lookupQuery jql
  runWithOptions $ runConduit (J.issueSearch jql' $= printer)
  return Nothing

lookupQuery :: MonadReader Options m => J.JQL -> m J.JQL
lookupQuery orig@(J.JQL t) =
  case T.split (== ':') t of
    ["jql", key] -> fromMaybe orig . M.lookup key <$> reader queries
    _            -> return orig

printer :: MonadIO m => Sink J.Issue m ()
printer = CL.mapM_ (liftIO . putStrLn . toS . J.formatIssue)

getActiveIssue :: (MonadState Log m) => m (Maybe (ZonedTime, IssueKey))
getActiveIssue = (toIssueKey <=< maybeLast) <$> get
  where toIssueKey (LogLine t (Started key)) = Just (t, key)
        toIssueKey _                         = Nothing
        maybeLast :: Log -> Maybe LogLine
        maybeLast [] = Nothing
        maybeLast xs = Just $ last xs

-- TODO: allow comments, or
-- maybe categories of work: i.e. code-review, development
-- TODO: nicer validation
start :: IssueKey -> Maybe TimeOffset -> CommandM (Maybe String)
start key offset' = do
  exists <- runWithOptions (J.issueExists key)
  if exists then do
    active <- getActiveIssue
    if isNothing active || (snd <$> active) /= Just key then do
      now <- liftIO getZonedTime
      let t = applyOffsetToZonedTime offset' now
      modify (<> [LogLine t (Started key)])
      return $ Just $ "Started working on " <> toS (toText key)
    else return $ Just $ "Already working on " <> toS (toText key)
  else throwError (toS (toText key) <> " does not exists")

applyOffsetToZonedTime :: Maybe TimeOffset -> ZonedTime -> ZonedTime
applyOffsetToZonedTime Nothing zt = zt
applyOffsetToZonedTime (Just offset) zt@(ZonedTime _ tz) =
  let utcTime = zonedTimeToUTC zt
      diff = fromInteger $ toSeconds offset
  in utcToZonedTime tz (addUTCTime diff utcTime)

stop :: Maybe TimeOffset -> CommandM (Maybe String)
stop offset' = do
  active <- getActiveIssue
  now <- liftIO getZonedTime
  case active of
    Just (_, key) -> do
      let t = applyOffsetToZonedTime offset' now
      modify (<> [LogLine t Stopped])
      return $ Just $ "Stopped working on " <> toS (toText key)
    Nothing ->
      return $ Just "No active issue"

review :: CommandM (Maybe String)
review = do
  active <- getActiveIssue
  (workLog, _) <- toWorkLog <$> get
  now <- liftIO getZonedTime
  revIssues <- reader reverseIssues
  F.putDoc $
    activeDoc revIssues active now F.</$>
    F.hardline F.<>
    workLogDoc revIssues workLog F.</$>
    F.hardline
  return Nothing
  where workLogDoc _ [] = F.text "Work log is empty"
        workLogDoc revIssues workLog =
          F.text "Work log to commit (" F.<>
          F.format (totalTime workLog) F.<> ")" F.</$>
          F.indent 4 (formatWorkLog revIssues workLog)
        activeDoc _ Nothing _ = F.text "No active issue"
        activeDoc revIssues (Just (t, key)) now =
          F.text "Active issue" F.</$>
          F.hardline F.<>
          F.indent 4 (formatWorkLogItem revIssues id $ createWorkLog key t now)
        totalTime workLog = mconcat $ timeSpent <$> workLog
        formatWorkLog revIssues workLog' =
          foldl (F.</$>)
            F.empty (formatWorkLogItem revIssues F.green `map` workLog')
        formatWorkLogItem revIssues good item =
          let fmt = F.format item
              line = case M.lookup (issueKey item) revIssues of
                Just [] -> fmt
                Just labels -> fmt F.<||> formatLabels labels
                Nothing -> fmt
          in if J.canBeBooked item then good line else F.red line
        formatLabels :: [T.Text] -> F.Doc
        formatLabels labels = F.text $ toS (T.unwords labels)

-- TODO: can this be made less convoluted?
book :: CommandM (Maybe String)
book = do
  (workLogLine', rest) <- nextWorkLogItem <$> get
  case workLogLine' of
    Just workLogLine ->
      if J.canBeBooked workLogLine
        then do
          result <- try (runWithOptions $ J.log workLogLine)
          case result of
            Left exc -> liftIO $ print (exc :: SomeException) >> return Nothing
            Right _ -> do
              put rest
              liftIO $ putStrLn $ toS (successMsg workLogLine)
              book
        else do
          liftIO $ putStrLn $ toS (invalidMsg workLogLine)
          put rest
          book
    Nothing -> put rest >> return Nothing
  where successMsg workLogLine =
          "Booked " <> toDurationString (timeSpent workLogLine)
                    <> " to " <> toText (issueKey workLogLine)
        invalidMsg workLogLine =
          "Discarded " <> toDurationString (timeSpent workLogLine)
                       <> " for " <> toText (issueKey workLogLine)

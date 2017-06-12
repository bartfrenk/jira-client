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
import qualified Data.Map.Strict          as Map
import           Data.Maybe               (fromMaybe, isNothing)
import           Data.Monoid              (mconcat)
import           Data.Semigroup           ((<>))
import           Data.String.Conv         (toS)
import qualified Data.Text                as T
import           Data.Time.Clock          (addUTCTime)
import           Data.Time.LocalTime
import           Prelude                  hiding (log)

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
    ["jql", key] -> fromMaybe orig . Map.lookup key <$> reader queries
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
      let t = applyOffsetToZonedTime now offset'
      modify (<> [LogLine t (Started key)])
      return $ Just $ "Started working on " <> toS (toText key)
    else return $ Just $ "Already working on " <> toS (toText key)
  else throwError (toS (toText key) <> " does not exists")

applyOffsetToZonedTime :: ZonedTime -> Maybe TimeOffset -> ZonedTime
applyOffsetToZonedTime zt Nothing = zt
applyOffsetToZonedTime zt@(ZonedTime _ tz) (Just offset) =
  let utcTime = zonedTimeToUTC zt
      diff = fromInteger $ toSeconds offset
  in utcToZonedTime tz (addUTCTime diff utcTime)

stop :: Maybe TimeOffset -> CommandM (Maybe String)
stop offset' = do
  active <- getActiveIssue
  now <- liftIO getZonedTime
  case active of
    Just (_, key) -> do
      let t = applyOffsetToZonedTime now offset'
      modify (<> [LogLine t Stopped])
      return $ Just $ "Stopped working on " <> toS (toText key)
    Nothing ->
      return $ Just "No active issue"

--  - Show gaps
--  - Show summary per day
review :: CommandM (Maybe String)
review = do
  active <- getActiveIssue
  (workLog, _) <- toWorkLog <$> get
  F.putDoc $
    activeDoc active F.</$>
    F.hardline F.<>
    workLogDoc (discardInvalid workLog) F.</$>
    F.hardline
  return Nothing
  where workLogDoc [] = F.text "Work log is empty"
        workLogDoc workLog =
          F.text "Work log to commit (" F.<>
          F.format (totalTime workLog) F.<> ")" F.</$>
          F.indent 4 (F.format workLog)
        activeDoc Nothing = F.text "No active issue"
        activeDoc (Just (t, key)) =
          F.text "Active issue since" F.<+> F.format t F.</$>
          F.hardline F.<>
          F.indent 4 (F.format key)
        totalTime workLog = mconcat $ timeSpent <$> workLog
        discardInvalid :: [WorkLog] -> [WorkLog]
        discardInvalid = filter J.canBeBooked

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

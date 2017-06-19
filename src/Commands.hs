{-# LANGUAGE OverloadedStrings #-}

module Commands where

import           Control.Applicative (optional)
import           Control.Monad       (mzero)
import           Data.Bifunctor      (first)
import qualified Data.Map.Strict     as M
import           Data.Semigroup      ((<>))
import           Data.String.Conv    (toS)
import           Data.Text           hiding (index, words)
import           Options.Applicative

import           Concepts
import qualified JIRA                as J
import           Options

data Command
  = Search J.JQL
  | Log IssueKey TimeSpent
  | Start IssueKey (Maybe TimeOffset)
  | Stop (Maybe TimeOffset)
  | Review
  | Book
  | Version
  | ListConfig

-- |If a number, read number and prepend prefix, otherwise read full issue key.
readIssueKey :: Text -> ReadM IssueKey
readIssueKey prefix = fromText <$> txtParser
  where index = toS . show <$> (auto :: ReadM Int)
        txtParser = (prefix <>) <$> index <|> toS <$> str

-- |Replace parsed issue alias with a key in `issueAliases`.
expandIssue :: M.Map Text IssueKey -> ReadM IssueKey
expandIssue issueAliases = do
  parsed <- str
  case M.lookup (toS parsed) issueAliases of
    Nothing  -> mzero
    Just key -> return key

parseIssueKey :: Options -> ReadM IssueKey
parseIssueKey opts =
  expandIssue (issues opts) <|> readIssueKey (defaultPrefix opts)

parseSearch :: Parser Command
parseSearch = Search <$> argument jql (metavar "JQL-QUERY")
  where jql = J.JQL . toS <$> str

parseStart :: Options -> Parser Command
parseStart opts = Start
  <$> argument (parseIssueKey opts) (metavar "ISSUE-KEY")
  <*> timeOffsetOption

timeOffsetOption :: Parser (Maybe TimeOffset)
timeOffsetOption =
  optional (option timeOffsetReader $ long "offset" <> metavar "TIME-OFFSET")
  where timeOffsetReader = eitherReader $ \s ->
          first show $ fromTimeOffsetString (toS s)

parseReview :: Parser Command
parseReview = pure Review

parseStop :: Parser Command
parseStop = Stop <$> timeOffsetOption

parseBook :: Parser Command
parseBook = pure Book

parseListConfig :: Parser Command
parseListConfig = pure ListConfig

parseLog :: Options -> Parser Command
parseLog opts = Log
  <$> argument (parseIssueKey opts) (metavar "ISSUE-KEY")
  <*> argument timeSpentReader (metavar "TIME-SPENT")
  where timeSpentReader = eitherReader $ \s ->
          first show $ fromDurationString (toS s)

withInfo :: Parser a -> String -> ParserInfo a
withInfo p desc = p `info` progDesc desc

defaultPrefix :: Options -> Text
defaultPrefix opts = defaultProject opts <> "-"

parseCommand :: Options -> Parser Command
parseCommand opts = hsubparser $
  command "search" (parseSearch `withInfo` "Search issues") <>
  command "log"    (parseLog opts `withInfo` "Create a new work log entry") <>
  command "start"  (parseStart opts `withInfo` "Start work on an issue") <>
  command "stop"   (parseStop `withInfo` "Stop work on active issue") <>
  command "review" (parseReview `withInfo` "Review logged work") <>
  command "book"   (parseBook `withInfo` "Book local worklog on JIRA") <>
  command "cfg"    (parseListConfig `withInfo` "List configuration")

parseVersionFlag :: Parser Command
parseVersionFlag = flag' Version (long "version" <> help "Show version")

runParser :: Options -> [String] -> IO Command
runParser opts args = handleParseResult $ execParserPure defaultPrefs p args
  where p = (helper <*> parseVersionFlag <|> parseCommand opts) `info`
            (fullDesc <> progDesc "JIRA command line client")

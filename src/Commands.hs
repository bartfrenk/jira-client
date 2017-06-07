{-# LANGUAGE OverloadedStrings #-}

module Commands where

import           Data.Bifunctor             (first)
import           Data.Semigroup             ((<>))
import           Data.String.Conv           (toS)
import           Data.Text                  hiding (index, words)
import           Options.Applicative

import           Concepts
import qualified JIRA                       as J
import           Options

data Command
  = Search J.JQL
  | Log IssueKey TimeSpent
  | Start IssueKey
  | Stop
  | Review
  | Book

-- | If a number, read number and prepend prefix, otherwise read full issue key.
readIssueKey :: Text -> ReadM IssueKey
readIssueKey prefix = fromText <$> txtParser
  where index = toS . show <$> (auto :: ReadM Int)
        txtParser = (prefix <>) <$> index <|> toS <$> str

parseSearch :: Parser Command
parseSearch = Search <$> argument jql (metavar "JQL-QUERY")
  where jql = J.JQL . toS <$> str

parseStart :: Text -> Parser Command
parseStart prefix =
  Start <$> argument (readIssueKey prefix) (metavar "ISSUE-KEY")

parseReview :: Parser Command
parseReview = pure Review

parseStop :: Parser Command
parseStop = pure Stop

parseBook :: Parser Command
parseBook = pure Book

parseLog :: Text -> Parser Command
parseLog prefix = Log
  <$> argument (readIssueKey prefix) (metavar "ISSUE-KEY")
  <*> argument timeSpentReader (metavar "TIME-SPENT")
  where timeSpentReader = eitherReader $ \s ->
          first show $ fromDurationString (toS s)

withInfo :: Parser a -> String -> ParserInfo a
withInfo p desc = p `info` progDesc desc

parseCommand :: Options -> Parser Command
parseCommand opts = hsubparser $
  command "search" (parseSearch `withInfo` "Search issues") <>
  command "log"    (parseLog prefix `withInfo` "Create a new work log entry") <>
  command "start"  (parseStart prefix `withInfo` "Start work on an issue") <>
  command "stop"   (parseStop `withInfo` "Stop work on active issue") <>
  command "review" (parseReview `withInfo` "Review logged work") <>
  command "book"   (parseBook `withInfo` "Book local worklog on JIRA")
  where prefix = defaultProject opts <> "-"

runParser :: Options -> [String] -> IO Command
runParser opts args = handleParseResult $ execParserPure defaultPrefs p args
  where p = (helper <*> parseCommand opts) `info`
            (fullDesc <> progDesc "JIRA command line client")

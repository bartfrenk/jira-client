{-# LANGUAGE OverloadedStrings #-}

module Commands where

import           Data.Semigroup      ((<>))
import           Data.String.Conv    (toS)
import           Data.Text           hiding (index, words)
import           Options.Applicative

import qualified JIRA                as J
import           Options

data Command
  = Search J.JQL
  | Log J.IssueKey J.TimeSpent
  | Start J.IssueKey
  | Review

-- | If a number, read number and prepend prefix, otherwise read full issue key.
readIssueKey :: Text -> ReadM J.IssueKey
readIssueKey prefix = (prefix <>) <$> index <|> toS <$> str
  where index = toS . show <$> (auto :: ReadM Int)

parseSearch :: Parser Command
parseSearch = Search <$> argument jql (metavar "JQL-QUERY")
  where jql = J.JQL . toS <$> str

parseStart :: Text -> Parser Command
parseStart prefix =
  Start <$> argument (readIssueKey prefix) (metavar "ISSUE-KEY")

parseReview :: Parser Command
parseReview = pure Review

parseLog :: Text -> Parser Command
parseLog prefix = Log
  <$> argument (readIssueKey prefix) (metavar "ISSUE-KEY")
  <*> argument timeSpent (metavar "TIME-SPENT")
  where timeSpent = J.TimeSpentCode . toS <$> str

withInfo :: Parser a -> String -> ParserInfo a
withInfo p desc = p `info` progDesc desc

parseCommand :: Options -> Parser Command
parseCommand opts = hsubparser $
  command "search" (parseSearch `withInfo` "Search issues") <>
  command "log"    (parseLog prefix `withInfo` "Create a new work log entry") <>
  command "start"  (parseStart prefix `withInfo` "Start work on an issue") <>
  command "review" (parseReview `withInfo` "Review logged work")
  where prefix = defaultProject opts <> "-"

runParser :: Options -> [String] -> IO Command
runParser opts args = handleParseResult $ execParserPure defaultPrefs p args
  where p = (helper <*> parseCommand opts) `info`
            (fullDesc <> progDesc "JIRA command line client")

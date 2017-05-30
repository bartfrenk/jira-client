{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Options where

import           Control.Exception
import           Data.Aeson          (withObject, (.:))
import           Data.ByteString     (ByteString, readFile)
import qualified Data.Map.Strict     as M
import           Data.Maybe          (fromMaybe)
import           Data.Semigroup      ((<>))
import           Data.String.Conv    (toS)
import           Data.Text           hiding (words, index)
import           Data.Typeable
import qualified Data.Yaml           as YAML
import           Options.Applicative hiding (execParser)
import           Prelude             hiding (readFile)

import qualified JIRA                as J

data Options = Options
  { baseURL        :: Text
  , password       :: Text
  , user           :: Text
  , defaultProject :: Text
  , queries        :: M.Map Text J.JQL
  , aliases        :: M.Map String [String]
  } deriving (Eq, Show)

instance YAML.FromJSON Options where
  parseJSON = withObject "Config" $ \v -> Options
    <$> v .: "baseURL"
    <*> v .: "password"
    <*> v .: "user"
    <*> v .: "defaultProject"
    <*> v .: "jql"
    <*> (words <$>) `fmap` (v .: "aliases")

data ParseException = ParseException ByteString
  deriving (Show, Typeable)

instance Exception ParseException

loadOptions :: Maybe FilePath -> IO Options
loadOptions path =
  let defaultPath = "./res/config.yaml"
  in do
    txt <- readFile (fromMaybe defaultPath path)
    maybe (throwIO $ ParseException txt) return (YAML.decode txt)

makeEnv :: Options -> J.Env
makeEnv Options{..} = J.Env
  { user = toS user
  , password = toS password
  , baseURL = toS baseURL }

data Command
  = Search J.JQL
  | Log J.IssueKey J.TimeSpent

parseSearch :: Parser Command
parseSearch = Search <$> argument jql (metavar "QUERY")
  where jql = J.JQL . toS <$> str

parseLog :: Text -> Parser Command
parseLog prefix = Log
  <$> argument issueKey (metavar "ISSUE")
  <*> argument timeSpent (metavar "TIME-SPENT")
  where index = toS . show <$> (auto :: ReadM Int)
        issueKey =  (prefix <>) <$> index <|> toS <$> str
        timeSpent = toS <$> str

withInfo :: Parser a -> String -> ParserInfo a
withInfo p desc = p `info` progDesc desc

parseCommand :: Options -> Parser Command
parseCommand opts = hsubparser $
  command "search" (parseSearch `withInfo` "Search issues") <>
  command "log"    (parseLog prefix `withInfo` "Create a new work log entry")
  where prefix = defaultProject opts <> "-"

runParser :: Options -> [String] -> IO Command
runParser opts args = handleParseResult $ execParserPure defaultPrefs p args
  where p = parseCommand opts `info` idm

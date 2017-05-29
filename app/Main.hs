{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Main where


import           Control.Monad          (join)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (withObject, (.:))
import qualified Data.Aeson             as JSON
import qualified Data.ByteString.Char8  as BS
import           Data.Conduit
import qualified Data.Conduit.List      as CL
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (fromMaybe, maybe)
import           Data.Semigroup         ((<>))
import           Data.String.Conv
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Yaml              as YAML
import           GHC.Generics
import           Options.Applicative
import           Prelude                hiding (log)
import           Prelude                hiding (log)
import           System.Environment     (getArgs, lookupEnv)

import qualified JIRA as JIRA

log :: Config -> LogOptions -> IO ()
log cfg LogOptions{..} = undefined


data SearchOptions = SearchOptions
  { jql :: JIRA.JQL
  } deriving (Eq, Show)

data LogOptions = LogOptions
  { issueKey  :: JIRA.IssueKey
  , timeSpent :: Text
  }

expandRefs :: Map Text JIRA.JQL -> JIRA.JQL -> JIRA.JQL
expandRefs refs jql@(JIRA.JQL t) =
  case Text.split (== ':') t of
    ["jql", key] -> fromMaybe jql (Map.lookup key refs)
    _            -> jql

getJiraEnv :: Config -> JIRA.Env
getJiraEnv Config{..} = JIRA.Env
  { user = toS user, password = toS password, baseURL = toS baseURL }

search :: Config -> SearchOptions -> IO ()
search cfg@Config{..} SearchOptions{..} = do
  let env = getJiraEnv cfg
  let conduit = JIRA.issueSearch (expandRefs queries jql) $= printer
  JIRA.runEnv (runConduit conduit) env

printer :: Sink JIRA.Issue JIRA.EnvM ()
printer = CL.mapM_ (liftIO . putStrLn . toS . formatIssue)

-- TODO: should go in separate formatter module
-- TODO: should make use of builder for performance
formatIssue :: JIRA.Issue -> Text
formatIssue issue =
     JIRA.key issue <> "\t"
  <> maybe "-" (toS . show) (JIRA.points issue) <> "\t"
  <> JIRA.summary issue

searchOptions :: Parser SearchOptions
searchOptions = SearchOptions . toS <$> strOption (short 'q' <>
                                   metavar "QUERY" <>
                                   help "Search issues using a JQL query")

logOptions :: Parser LogOptions
logOptions = LogOptions
  <$> argument txt (metavar "ISSUE" <> help "Help")
  <*> argument txt (metavar "TIME" <> help "Help")
  where txt = toS <$> str

opts :: Config -> Parser (IO ())
opts cfg = hsubparser (
    command "search" (info (search cfg <$> searchOptions) mempty)
 <> command "log"  (info (log cfg <$> logOptions) mempty))

type Aliases = Map Text [Text]

expandAliases :: Aliases -> [Text] -> [Text]
expandAliases aliases args =
  args >>= (\s -> Map.findWithDefault [s] s aliases)

execParserWithArgs :: ParserInfo a -> [Text] -> IO a
execParserWithArgs parser args =
  handleParseResult $ execParserPure defaultPrefs parser (toS <$> args)

defaultAliases :: Aliases
defaultAliases = Map.fromList [("current", ["search", "-q", "hello hello"])]

data Config = Config
  { baseURL  :: Text
  , password :: Text
  , user     :: Text
  , queries  :: Map Text JIRA.JQL
  , aliases  :: Map Text [Text]
  } deriving (Eq, Show, Generic)

instance YAML.FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
    <$> v .: "baseURL"
    <*> v .: "password"
    <*> v .: "user"
    <*> v .: "jql"
    <*> (Text.words <$>) `fmap` (v .: "aliases")

run :: Config -> [String] -> IO ()
run cfg args =
  let args' = expandAliases (aliases cfg) (toS <$> args)
  in join $ execParserWithArgs (info (opts cfg) mempty) args'

loadConfig :: JSON.FromJSON a => FilePath -> IO (Maybe a)
loadConfig path = YAML.decode <$> BS.readFile path

defaultConfigPath :: FilePath
defaultConfigPath = "./res/config.yaml"

main :: IO ()
main = do
  args <- getArgs
  path <- fromMaybe defaultConfigPath <$> lookupEnv "JIRA_CLIENT_CONFIG"
  cfg' <- loadConfig path
  case cfg' of
    Just cfg ->
      run cfg args
    Nothing  ->
      putStrLn $ "Could not load configuration file at " ++ toS path

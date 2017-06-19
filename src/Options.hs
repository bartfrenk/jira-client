{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Options (Options(..),
                loadOptions,
                reverseIssues,
                makeEnv) where

import           Control.Exception
import           Data.Aeson          (withObject, (.:))
import           Data.ByteString     (readFile)
import qualified Data.Map.Strict     as M
import           Data.Maybe          (fromMaybe)
import           Data.String.Conv    (toS)
import           Data.Text           hiding (index, words)
import qualified Data.Yaml           as YAML
import           Options.Applicative hiding (execParser, runParser)
import           Prelude             hiding (readFile)
import           System.Directory    (getHomeDirectory)
import           System.FilePath     ((</>))

import qualified Concepts as C
import           Core
import qualified JIRA                as J

data Options = Options
  { baseURL        :: Text
  , password       :: Text
  , user           :: Text
  , defaultProject :: Text
  , issues         :: M.Map Text C.IssueKey
  , queries        :: M.Map Text J.JQL
  , aliases        :: M.Map String [String]
  } deriving (Eq, Show)

invert :: (Ord k, Ord v) => M.Map k [v] -> M.Map v [k]
invert m = M.fromListWith (++) pairs
    where pairs = [(v, [k]) | (k, vs) <- M.toList m, v <- vs]

reverseIssues :: Options -> M.Map C.IssueKey [Text]
reverseIssues = invert . fmap return . issues

instance YAML.FromJSON Options where
  parseJSON = withObject "Config" $ \v -> Options
    <$> v .: "baseURL"
    <*> v .: "password"
    <*> v .: "user"
    <*> v .: "defaultProject"
    <*> v .: "issues"
    <*> v .: "jql"
    <*> (words <$>) `fmap` (v .: "aliases")

loadOptions :: Maybe FilePath -> IO Options
loadOptions path =
  let defaultFile = ".jira-cli/config.yaml"
  in do
    home <- getHomeDirectory
    txt <- readFile (fromMaybe (home </> defaultFile) path)
    maybe (throwIO $ ParseException txt) return (YAML.decode txt)

makeEnv :: Options -> J.Env
makeEnv Options{..} = J.Env
  { user = toS user
  , password = toS password
  , baseURL = toS baseURL }


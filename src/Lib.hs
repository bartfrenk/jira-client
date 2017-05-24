{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
module Lib where

import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Aeson.Lens        as J
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as C8
import           Data.Monoid
import           Data.String.Conv
import           Data.Text              (Text)
import           GHC.Generics
import           Network.HTTP.Types
import           Network.Wreq
import           Prelude                hiding (log)



data WorkLogLine = WorkLogLine
  { timeSpent :: String
  }

url' :: String
url' = "http://camelot.bluemango.nl/rest/api/2/issue/lemonpi-3220/worklog"

data Env = Env
  { user     :: ByteString
  , password :: ByteString
  , baseURL  :: ByteString
  }

defaultEnv :: Env
defaultEnv = Env
  { user = "bart.frenk"
  , password = ""
  , baseURL = "http://camelot.bluemango.nl/rest/api/2"
  }


type EnvM = ReaderT Env IO

withCredentials :: MonadReader Env m => Options -> m Options
withCredentials opts = do
  user' <- reader user
  password' <- reader password
  return $ opts & auth ?~ basicAuth user' password'
                & header "Content-Type" .~ ["application/json"]

log :: Issue -> WorkLogLine -> EnvM Status
log _ _ = do
  opts <- withCredentials defaults
  response <- liftIO $ getWith opts url'
  return $ response ^. responseStatus

getTotalTime :: EnvM Integer
getTotalTime =
  sum . toListOf times <$> fromJIRA GET "/issue/lemonpi-3220/worklog" []
    where
      times = J.key "worklogs" . J.values . J.key "timeSpentSeconds" . J._Integer

type JQL = ByteString

type Path = ByteString

data Issue = Issue
  { key     :: Text
  , id      :: Text
  , summary :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON Issue where
  parseJSON = withObject "Issue" $ \v -> Issue
      <$> v .: "key"
      <*> v .: "id"
      <*> ((v .: "fields") >>= (.: "summary"))

instance ToJSON Issue

searchIssues :: JQL -> EnvM [Issue]
searchIssues jql =
  let query = [("jql", Just jql), ("fields", Just "key,summary")]
      issues = J.key "issues" . J.values . J._JSON
  in toListOf issues <$> fromJIRA GET "/search" query

issuesInCurrentSprint :: EnvM [Issue]
issuesInCurrentSprint = searchIssues "sprint in openSprints() \
                                     \and sprint not in futureSprints() \
                                     \and project=LemonPI"


replace :: Char -> Char -> ByteString -> ByteString
replace old new = C8.map (\c -> if c == old then new else c)

createURL :: Path -> Query -> EnvM ByteString
createURL path query =
  let queryString = replace ' ' '+' $ urlDecode False (renderQuery True query)
  in (<> path <> queryString) <$> reader baseURL

fromJIRA :: StdMethod -> Path -> Query -> EnvM ByteString
fromJIRA method path query = do
  opts <- withCredentials defaults
  url <- createURL path query
  response <- liftIO $ customMethodWith (show method) opts (toS url)
  return $ response ^. responseBody . strict

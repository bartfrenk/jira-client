{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module JIRA (runEnv,
             fromJIRA,
             searchIssues,
             issuesInCurrentSprint,
             EnvM,
             Env(..),
             Issue(..),
             JQL(..)) where

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

data Env = Env
  { user     :: ByteString
  , password :: ByteString
  , baseURL  :: ByteString
  }

type EnvM = ReaderT Env IO

runEnv :: EnvM a -> Env -> IO a
runEnv = runReaderT

withCredentials :: MonadReader Env m => Options -> m Options
withCredentials opts = do
  user' <- reader user
  password' <- reader password
  return $ opts & auth ?~ basicAuth user' password'
                & header "Content-Type" .~ ["application/json"]

newtype JQL = JQL Text deriving (Eq, Show)

instance FromJSON JQL where
  parseJSON = (JQL <$>) . parseJSON

instance StringConv String JQL where
  strConv leniency s = JQL $ strConv leniency s

instance StringConv JQL ByteString where
  strConv leniency (JQL t) = strConv leniency t

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
searchIssues (JQL t) =
  let query = [("jql", Just $ toS t), ("fields", Just "key,summary")]
      issues = J.key "issues" . J.values . J._JSON
  in toListOf issues <$> fromJIRA GET "/search" query

issuesInCurrentSprint :: EnvM [Issue]
issuesInCurrentSprint = searchIssues $
  JQL "sprint in openSprints() \
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

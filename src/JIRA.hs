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
             issueSearch,
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
import           Data.Conduit           (Source, yield)
import           Data.Monoid
import           Data.String.Conv
import           Data.Text              (Text)
import           GHC.Generics
import           Network.HTTP.Types
import           Network.Wreq
import           Prelude                hiding (log)

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
  , points  :: Maybe Double
  } deriving (Eq, Show, Generic)

instance FromJSON Issue where
  parseJSON = withObject "Issue" $ \v -> Issue
      <$> v .: "key"
      <*> v .: "id"
      <*> ((v .: "fields") >>= (.: "summary"))
      <*> ((v .: "fields") >>= (.: "customfield_10002"))

instance ToJSON Issue


data Env = Env
  { user     :: ByteString
  , password :: ByteString
  , baseURL  :: ByteString
  }

type EnvT = ReaderT Env

type EnvM = EnvT IO


runEnv :: EnvT m a -> Env -> m a
runEnv = runReaderT

withCredentials :: MonadReader Env m => Options -> m Options
withCredentials opts = do
  user' <- reader user
  password' <- reader password
  return $ opts & auth ?~ basicAuth user' password'
                & header "Content-Type" .~ ["application/json"]

issueSearch :: JQL -> Source EnvM Issue
issueSearch jql = loop 0
  where
    limit = 200
    loop offset = do
      issues <- lift $ fetchIssues jql offset limit
      mapM_ yield issues
      unless (null issues) $
        loop (offset + length issues)

fetchIssues :: (FromJSON a, ToJSON a) => JQL -> Int -> Int -> EnvM [a]
fetchIssues (JQL t) offset limit =
  let query = [("jql", Just $ toS t),
               -- customfield_10002 is story points for the LemonPI instance
               ("fields", Just "key,summary,issuetype,customfield_10002"),
               ("startAt", Just $ toS $ show offset),
               ("maxResults", Just $ toS $ show limit)]
      issues = J.key "issues" . J.values . J._JSON
  in toListOf issues <$> fromJIRA GET "/search" query

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

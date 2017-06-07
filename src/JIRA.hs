{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module JIRA (runEnv,
             issueSearch,
             issueExists,
             formatIssue,
             log,
             canBeBooked,
             EnvM,
             Env(..),
             Issue(..),
             JQL(..)) where

import           Control.Exception             (throwIO)
import           Control.Lens
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Aeson.Lens               as J
import           Data.ByteString               (ByteString)
import qualified Data.ByteString.Char8         as C8
import qualified Data.ByteString.Lazy.Internal as L
import           Data.Conduit                  (Source, yield)
import qualified Data.HashMap.Lazy             as M
import           Data.Monoid
import           Data.String.Conv
import qualified Data.Text                     as T
import           Data.Time.Format
import           GHC.Generics
import           Network.HTTP.Client           (HttpException (..),
                                                HttpExceptionContent (..))
import           Network.HTTP.Types            (Query, renderQuery, urlDecode)
import           Network.Wreq                  hiding (get, post)
import qualified Network.Wreq                  as W
import           Network.Wreq.Types            (Postable)
import           Prelude                       hiding (log)

import           Concepts

newtype JQL = JQL T.Text deriving (Eq, Show)

instance FromJSON JQL where
  parseJSON = (JQL <$>) . parseJSON

instance StringConv String JQL where
  strConv leniency s = JQL $ strConv leniency s

instance StringConv JQL ByteString where
  strConv leniency (JQL t) = strConv leniency t
type Path = ByteString

data Issue = Issue
  { key     :: IssueKey
  , id      :: T.Text
  , summary :: T.Text
  , points  :: Maybe Double
  } deriving (Eq, Show, Generic)


-- TODO: should make use of builder for performance
formatIssue :: Issue -> T.Text
formatIssue issue =
     toText (key issue) <> "\t"
  <> maybe "-" (toS . show) (points issue) <> "\t"
  <> summary issue

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
  } deriving (Eq, Show)

type EnvT = ReaderT Env

type EnvM = EnvT IO

runEnv :: EnvT m a -> Env -> m a
runEnv = runReaderT

withCredentials :: MonadReader Env m => W.Options -> m W.Options
withCredentials opts = do
  user' <- toS <$> reader user
  password' <- toS <$> reader password
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
      issues = responseBody . J.key "issues" . J.values . J._JSON
  in toListOf issues <$> (get defaults =<< createURL "/search" query)

replace :: Char -> Char -> ByteString -> ByteString
replace old new = C8.map (\c -> if c == old then new else c)

createURL :: Path -> Query -> EnvM ByteString
createURL path query =
  let queryString = replace ' ' '+' $ urlDecode False (renderQuery True query)
  in (<> "/rest/api/2" <> path <> queryString) <$> reader baseURL

-- TODO: pull in createURL
get :: Options -> Path -> EnvM (Response L.ByteString)
get base url = do
  opts <- withCredentials base
  liftIO $ getWith opts (toS url)

post :: Postable a => Path -> a -> EnvM ByteString
post url payload = do
  opts <- withCredentials defaults
  response <- liftIO $ postWith opts (toS url) payload
  return $ response ^. responseBody . strict

-- TODO: check response
issueExists :: IssueKey -> EnvM Bool
issueExists issueKey = do
  url <- createURL ("/issue/" <> toS (toText issueKey)) []
  response <- get (defaults & checkResponse .~ Just ignore) url
  case response ^. responseStatus . statusCode of
    200 -> return True
    _   -> return False
  where mkException req resp =
          HttpExceptionRequest req $ StatusCodeException (void resp) ""
        ignore req resp =
          case resp ^. responseStatus . statusCode of
            200 -> return ()
            404 -> return ()
            _   -> throwIO $ mkException req resp

-- TODO: send duration string instead of seconds (gives better results in JIRA
-- API)
log :: WorkLog -> EnvM ()
log WorkLog{..} = do
  path <- createURL ("/issue/" <> toS (toText issueKey) <> "/worklog") []
  _ <- post path payload
  return ()
  where
    payload = encode $ Object $
      M.fromList [("started", toJSON $ formatTime defaultTimeLocale formatStr started),
                  ("timeSpentSeconds", toJSON $ toSeconds timeSpent)]
    formatStr = iso8601DateFormat (Just "%H:%M:%S.000%z")

canBeBooked :: WorkLog -> Bool
canBeBooked = (60 <=) . toSeconds . timeSpent

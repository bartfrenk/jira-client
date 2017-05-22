{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib where

import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString        as BS
import           Data.String.Conv
import           Network.Wreq
import           Prelude                hiding (log)


data WorkLogLine = WorkLogLine
  { timeSpent :: String
  }

url :: String
url = "http://camelot.bluemango.nl/rest/api/2/issue/lemonpi-3220/worklog"

data Env = Env
  { user     :: BS.ByteString
  , password :: BS.ByteString
  , baseURL :: String
  }

defaultEnv :: Env
defaultEnv = Env
  { user = "bart.frenk"
  , password = ""
  , baseURL = "http://camelot.bluemango.nl/rest/api/2"
  }

data Issue = LemonPI Int

toPathParam :: Issue -> String
toPathParam (LemonPI n) = "lemonpi-" ++ show n

type EnvM = ReaderT Env IO

withCredentials :: MonadReader Env m => Options -> m Options
withCredentials opts = do
  user <- reader user
  password <- reader password
  return $ opts & auth ?~ basicAuth user password
                    & header "Content-Type" .~ ["application/json"]

log :: Issue -> WorkLogLine -> EnvM Status
log _ _ = do
  opts <- withCredentials defaults
  response <- liftIO $ getWith opts url
  return $ response ^. responseStatus

getTotalTime :: Issue -> EnvM Integer
getTotalTime _ = do
  opts <- withCredentials defaults
  response <- liftIO $ getWith opts url
  return $ sum $ response ^.. responseBody . totalTime
    where totalTime =
            key "worklogs" . values . key "timeSpentSeconds" . _Integer

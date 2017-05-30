{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Main where


import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Conduit
import qualified Data.Conduit.List      as CL
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (fromMaybe, maybe)
import           Data.Semigroup         ((<>))
import           Data.String.Conv       (toS)
import           Data.Text              (Text, split)
import           Prelude                hiding (log)
import           System.Environment     (getArgs, lookupEnv)

import qualified JIRA                   as J
import           Options

log :: Options -> J.IssueKey -> J.TimeSpent -> IO ()
log opts issueKey timeSpent = void $ J.runEnv act (makeEnv opts)
  where act = J.log issueKey $ J.WorkLog timeSpent Nothing

search :: Options -> J.JQL -> IO ()
search cfg jql = J.runEnv (runConduit conduit) (makeEnv cfg)
  where expanded = expandQueries (queries cfg) jql
        conduit = J.issueSearch expanded $= printer
        expandQueries refs orig@(J.JQL t) =
          case split (== ':') t of
            ["jql", key] -> fromMaybe orig (Map.lookup key refs)
            _            -> orig

printer :: Sink J.Issue J.EnvM ()
printer = CL.mapM_ (liftIO . putStrLn . toS . formatIssue)

-- TODO: should go in separate formatter module
-- TODO: should make use of builder for performance
formatIssue :: J.Issue -> Text
formatIssue issue =
     J.key issue <> "\t"
  <> maybe "-" (toS . show) (J.points issue) <> "\t"
  <> J.summary issue

expand :: Ord a => Map a [a] -> [a] -> [a]
expand m xs =
  xs >>= (\s -> Map.findWithDefault [s] s m)

run :: Options -> Command -> IO ()
run opts cmd =
  case cmd of
    Search jql             -> search opts jql
    Log issueKey timeSpent -> log opts issueKey timeSpent

main :: IO ()
main = do
  opts <- loadOptions =<< lookupEnv "JIRA_CLIENT_CONFIG"
  args <- getArgs
  cmd <- runParser opts $ expand (aliases opts) args
  run opts cmd

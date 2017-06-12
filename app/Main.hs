{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Main where


import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Prelude                      hiding (log)
import           System.Environment           (getArgs, lookupEnv)
import qualified Text.PrettyPrint.ANSI.Leijen as P

import           Commands
import           Options
import           Run
import           State


expand :: Ord a => Map a [a] -> [a] -> [a]
expand m xs =
  xs >>= (\s -> Map.findWithDefault [s] s m)


action :: Command -> CommandM (Maybe String)
action (Search jql)             = search jql
action (Log issueKey timeSpent) = log issueKey timeSpent
action (Start issueKey offset') = start issueKey offset'
action Review                   = review
action (Stop offset')           = stop offset'
action Book                     = book
action Version                  = printVersion

run :: Command -> Options -> IO (Either Failure (Maybe String))
run cmd opts = do
  previous <- loadState Nothing
  (result, next) <- runCommandM (action cmd) opts previous
  writeState Nothing next
  return result

putDocLn :: P.Doc -> IO ()
putDocLn doc = do
  P.putDoc doc
  putStrLn ""

printResult :: Either Failure (Maybe String) -> IO ()
printResult (Left txt)         = putDocLn $ P.red (P.text txt)
printResult (Right (Just txt)) = putDocLn (P.text txt)
printResult (Right Nothing)    = return ()

main :: IO ()
main = do
  opts <- loadOptions =<< lookupEnv "JIRA_CLIENT_CONFIG"
  args <- getArgs
  cmd <- runParser opts $ expand (aliases opts) args
  run cmd opts >>= printResult

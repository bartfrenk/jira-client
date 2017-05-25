{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Data.String.Conv
import Control.Monad (join)
import Data.Text
import System.Environment (getArgs)

stop :: IO ()
stop = putStrLn "stop"

newtype JQL = JQL Text deriving (Eq, Show)

instance StringConv String JQL where
  strConv leniency s = JQL $ strConv leniency s

data SearchOptions = SearchOptions
  { jql :: JQL
  } deriving (Eq, Show)

search :: SearchOptions -> IO ()
search = print

searchOptions :: Parser SearchOptions
searchOptions = SearchOptions . toS <$> strOption (short 'q' <>
                                   metavar "QUERY" <>
                                   help "Search issues using a JQL query")

opts :: Parser (IO ())
opts = hsubparser (
    command "search" (info (search <$> searchOptions) mempty)
 <> command "stop"  (info (pure stop) mempty))

type Aliases = Map String [String]

expandAliases :: Aliases -> [String] -> [String]
expandAliases aliases args =
  args >>= (\s -> Map.findWithDefault [s] s aliases)

execParserWithArgs :: ParserInfo a -> [String] -> IO a
execParserWithArgs parser args =
  handleParseResult $ execParserPure defaultPrefs parser args

defaultAliases :: Aliases
defaultAliases = Map.fromList [("current", ["search", "-q", "hello hello"])]

main :: IO ()
main = do
  args <- expandAliases defaultAliases <$> getArgs
  join $ execParserWithArgs (info opts mempty) args

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
module Concepts where

import           Data.Aeson
import           Data.Semigroup      ((<>))
import           Data.String.Conv    (toS)
import qualified Data.Text           as T
import           Data.Time.Clock     (diffUTCTime)
import           Data.Time.LocalTime
import           GHC.Generics        (Generic)
import           Prelude             hiding (Read)
import           Text.Parsec

import           Orphans             ()
import           Utils               (breakdown)

data WorkLog = WorkLog
  { issueKey  :: IssueKey
  , timeSpent :: TimeSpent
  , started   :: ZonedTime
  } deriving (Show, Generic, Eq)

createWorkLog :: IssueKey -> ZonedTime -> ZonedTime -> WorkLog
createWorkLog key start finish =
  WorkLog key (computeTimeSpent start finish) start

newtype TimeSpent = TimeSpent { toSeconds :: Integer } deriving (Eq)

type TimeOffset = TimeSpent

instance Monoid TimeSpent where
  mempty = TimeSpent 0
  (TimeSpent s) `mappend` (TimeSpent t) = TimeSpent (s + t)

instance Show TimeSpent where
  show (TimeSpent s) = show s

fromSeconds :: Integer -> TimeSpent
fromSeconds = TimeSpent

computeTimeSpent :: ZonedTime -> ZonedTime -> TimeSpent
computeTimeSpent start finish =
  let delta = diffUTCTime (zonedTimeToUTC finish) (zonedTimeToUTC start)
  in fromSeconds $ truncate (realToFrac delta :: Double)

-- |Parser to convert strings of the form '1h3d2m1s' to seconds.
durationStringParser :: Parsec T.Text u Integer
durationStringParser = sum <$> sepBy1 seconds whitespace
  where number = read <$> many1 digit
        seconds = countSeconds <$> number <*> oneOf ['d', 'h', 'm', 's']
        whitespace = many $ char ' '
        countSeconds :: Integer -> Char -> Integer
        countSeconds n 'd' = n * 24 * 60 * 60
        countSeconds n 'h' = n * 60 * 60
        countSeconds n 'm' = n * 60
        countSeconds n 's' = n
        countSeconds _ _   = error ""

-- |Reads from a string of the form '1h3d2m1s' ignoring spaces, and summing
-- duplicate entries.
fromDurationString :: T.Text -> Either ParseError TimeSpent
fromDurationString = parse (fromSeconds <$> durationStringParser) ""

fromTimeOffsetString :: T.Text -> Either ParseError TimeOffset
fromTimeOffsetString = parse (fromSeconds <$> timeOffsetStringParser) ""
  where timeOffsetStringParser = (*) <$> minus <*> durationStringParser
        minus = option 1 (char '-' >> return (-1))

-- |Maps to a string of the form '1h3d2m1s'.
toDurationString :: TimeSpent -> T.Text
toDurationString = T.concat . toDurationList

-- |Maps to a list of the form [2d, 3h, 2m, 1s], without zero entries.
toDurationList :: TimeSpent -> [T.Text]
toDurationList (TimeSpent s) | s >= 0 =
  let seconds = scanr (*) 1 [24, 60, 60]
      periods = ["d", "h", "m", "s"]
      perPeriod = breakdown s seconds
      nonZero = filter ((/= 0) . fst) $ zip perPeriod periods
  in (\(n, per) -> toS $ show n <> per) <$> nonZero
toDurationList (TimeSpent s)
  | s < 0 = "-" : toDurationList (TimeSpent (-s))
toDurationList (TimeSpent _) = undefined -- does not happen

newtype IssueKey = IssueKey T.Text

instance Show IssueKey where
  show (IssueKey txt) = toS $ T.toUpper txt

instance Eq IssueKey where
  (IssueKey t) == (IssueKey s) = T.toUpper t == T.toUpper s

instance FromJSON IssueKey where
  parseJSON = (IssueKey <$>) . parseJSON

instance ToJSON IssueKey where
  toJSON (IssueKey txt) = toJSON $ T.toUpper txt

toText :: IssueKey -> T.Text
toText (IssueKey txt) = T.toUpper txt

fromText :: T.Text -> IssueKey
fromText = IssueKey

{-# OPTIONS_GHC -fno-warn-orphans #-}

module TimeGenerators where

import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.LocalTime
import           System.Random
import           Test.QuickCheck

instance Random Day where
  random g =
    let (n, g') = random g
    in (ModifiedJulianDay n, g')
  randomR (s, t) g =
    let (n, g') = randomR (toModifiedJulianDay s, toModifiedJulianDay t) g
    in (ModifiedJulianDay n, g')

arbitraryUTCTime :: (Day, Day) -> Gen UTCTime
arbitraryUTCTime period = do
  day <- choose period
  diff <- secondsToDiffTime <$> choose (0, 86401)
  return $ UTCTime day diff

arbitraryZonedTime :: TimeZone -> (Day, Day) -> Gen ZonedTime
arbitraryZonedTime tz period =
  utcToZonedTime <$> return tz <*> arbitraryUTCTime period


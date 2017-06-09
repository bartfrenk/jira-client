{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module StateSpec where

import           Control.Monad       (liftM2)
import           Data.DeriveTH
import           Data.List           (sort)
import           Data.Semigroup      ((<>))
import           Data.String.Conv
import           Data.Time.Calendar  (Day, fromGregorian)
import           Data.Time.LocalTime (utc)
import           Test.Hspec
import           Test.QuickCheck

import           Concepts
import           State
import           TimeGenerators

instance Arbitrary IssueKey where
  arbitrary = fromText <$> ((<>) <$> projectPrefix <*> index)
    where projectPrefix = elements ["Foo-", "Bar-", "Baz-"]
          index = toS . show <$> (choose (1, 1000) :: Gen Int)

derive makeArbitrary ''LogLineType

arbitraryLogLine :: (Day, Day) -> Gen LogLine
arbitraryLogLine period =
  LogLine <$> arbitraryZonedTime utc period <*> arbitrary

today :: Day
today = fromGregorian 2017 6 9

-- |Creates a generator for valid logs with time entries in the specified time interval.
validLog' :: (Day, Day) -> Gen Log
validLog' period =
  process <$> listOf (arbitraryLogLine period)
  where prune (LogLine _ Stopped:keep@(LogLine _ Stopped):rest)
          = prune (keep:rest)
        prune (keep:rest)
          = keep:prune rest
        prune [] = []
        isStopped (LogLine _ Stopped) = True
        isStopped _                   = False
        process = prune . dropWhile isStopped . sort

-- |As `validLog'`, but ensures the generated log is non-empty.
nonEmptyValidLog' :: (Day, Day) -> Gen Log
nonEmptyValidLog' period = validLog' period `suchThat` (not . null)

-- |A log representing a valid workflow, without repeated stops, and with increasing time
-- entries.
validLog :: Gen Log
validLog = validLog' (today, today)

-- |Generator for logs, than when concatenated, still form a valid log.
subsequentLogs :: Gen (Log, Log)
subsequentLogs =
  let t = fromGregorian 2017 6 9
      s = fromGregorian 2017 6 10
      paired = liftM2 (,) (nonEmptyValidLog' (t, t)) (nonEmptyValidLog' (s, s))
  in paired `suchThat` \(p, q) -> last p < last q

spec :: Spec
spec =
  describe "toWorkLog" $ do

    it "should keep the last item if it is a 'started' entry" $
      forAll validLog $ \log -> not (null log) && isStarted (last log) ==>
                                let (_, rest) = toWorkLog log
                                in [last log] == rest

    it "remaining event log items should be independent of prefix" $
      forAll subsequentLogs $ \(p, q) ->
        last p < last q ==> snd (toWorkLog $ p ++ q) == snd (toWorkLog q)

    it "appending to a closed event log should give appended work logs" $
      forAll subsequentLogs $ \(p, q) ->
        not (isStarted (last p)) ==>
          fst (toWorkLog p) ++ fst (toWorkLog q) == fst (toWorkLog $ p ++ q)

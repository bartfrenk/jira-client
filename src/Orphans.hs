{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans where

import Data.Time.LocalTime

instance Eq ZonedTime where
  x == y = zonedTimeToUTC x == zonedTimeToUTC y

instance Ord ZonedTime where
  x `compare` y = zonedTimeToUTC x `compare` zonedTimeToUTC y


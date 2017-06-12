{-# LANGUAGE RecordWildCards #-}
module Format (module Text.PrettyPrint.ANSI.Leijen,
               module Format) where

import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Data.String.Conv             (toS)
import           Data.Time.Format
import           Data.Time.LocalTime
import           Text.PrettyPrint.ANSI.Leijen hiding (putDoc, (<$>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           Concepts

(</$>) :: Doc -> Doc -> Doc
(</$>) = (PP.<$>)

putDoc :: MonadIO m => Doc -> m ()
putDoc = liftIO . PP.putDoc

class Format a where
  format :: a -> Doc

instance Format TimeSpent where
  format = text . toS . toDurationString

instance Format a => Format [a] where
  format xs = foldl (</$>) empty (format `map` xs)

instance Format IssueKey where
  format issueKey = text $ show issueKey

(<||>) :: Doc -> Doc -> Doc
first <||> second = first <> text "\t" <> second

instance Format WorkLog where
  format WorkLog{..} =
    format issueKey <||> format started <||> format timeSpent

instance Format ZonedTime where
  format = text . formatTime defaultTimeLocale formatStr
    where formatStr = "%Y-%m-%d %H:%M"

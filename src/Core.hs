module Core where

import           Control.Exception
import           Data.ByteString
import           Data.Typeable


data ParseException = ParseException ByteString
  deriving (Show, Typeable)


instance Exception ParseException

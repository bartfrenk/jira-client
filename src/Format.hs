module Format (module Text.PrettyPrint.ANSI.Leijen,
               module Format) where

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

(</$>) :: Doc -> Doc -> Doc
(</$>) = (PP.<$>)

class Format a where
  format :: a -> Doc

instance Format a => Format [a] where
  format xs = foldl (</$>) empty (format `map` xs)

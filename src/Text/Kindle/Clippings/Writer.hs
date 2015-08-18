module Text.Kindle.Clippings.Writer 
( showClipping
, showClippings
) where

import Data.Time.Format (formatTime)
import Data.Time.LocalTime (LocalTime)
import System.Locale (defaultTimeLocale)
import Text.Kindle.Clippings.Types (Clipping(..), Document(..), Position(..), Content(..), Interval(..))

instance Show Document where
  show (Document t (Just a)) = t ++ " (" ++ a ++ ")"
  show (Document t Nothing)  = t

instance Show Interval where
  show (Singleton i) = show i
  show (Proper i0 i1) = show i0 ++ "-" ++ show i1

instance Show Position where
  show (Position Nothing (Just l)) = "Loc. " ++ show l
  show (Position (Just p) Nothing) = "on Page " ++ show p
  show (Position p@(Just _) l@(Just _)) = show (Position p Nothing) ++ " | " ++ show (Position Nothing l)
  show (Position Nothing Nothing) = ""

instance Show Content where
  show (Highlight s) = s
  show (Annotation s)  = s
  show (Bookmark) = ""

showContentType :: Content -> String
showContentType (Highlight _) = "Highlight"
showContentType (Annotation _) = "Note"
showContentType (Bookmark) = "Bookmark"

showKindleDate :: LocalTime -> String
showKindleDate = formatTime defaultTimeLocale "%A, %d %B %y %X"

showClipping :: Clipping -> String
showClipping c = unlines $
  [ show (document c)
  , "- " ++ showType c ++ " " ++ showPosition c ++ " | Added on " ++ showDate c
  , ""
  , showContent c
  , "=========="
  ] where showPosition = show . position 
          showDate = showKindleDate . date 
          showContent = show . content 
          showType = showContentType . content

instance Show Clipping where
  show = showClipping

showClippings :: [Clipping] -> String
showClippings = concat . map showClipping

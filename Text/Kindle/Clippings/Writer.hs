module Text.Kindle.Clippings.Writer 
( showClipping
, showClippings
) where

import Data.Time.LocalTime (LocalTime)
import Data.Time.Format (formatTime, readTime)
import System.Locale (defaultTimeLocale)
import Text.Kindle.Clippings.Types

instance Show Document where
  show (Document title (Just author)) = title ++ " (" ++ author ++ ")"
  show (Document title (Nothing))     = title

instance Show Position where
  show (Position Nothing (Just l))  = show l
  show (Position Nothing Nothing)   = ""
  show (Position (Just p) (Just l)) = "on Page " ++ show p ++ " | " ++ show l
  show (Position (Just p) Nothing)  = "on Page " ++ show p 

instance Show Content where
  show (Highlight s) = s
  show (Annotation s)  = s
  show (Bookmark) = ""

instance Show Location where
  show (Location i) = "Loc. " ++ show i
  show (Region (l1,l2)) = "Loc. " ++ show l1 ++ "-" ++ show l2

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

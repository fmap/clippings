module Text.Kindle.Clippings.Types where

import Data.Time.LocalTime (LocalTime)
import Data.Time.Parse (strptime)
import Data.Time.Format (formatTime, readTime)
import Data.Maybe (fromMaybe)
import System.Locale (defaultTimeLocale)

data Clipping = Clipping
  { document :: Document
  , position :: Position
  , date     :: LocalTime
  , content  :: Content
  } 

data Document = Document 
  { title  :: String
  , author :: Maybe String -- Not always present, e.g. "Oxford Dictionary of English\n".
  }

data Position = Position
  { page     :: Maybe Int
  , location :: Maybe Location -- PDFs don't get these.
  }

data Location = Location Int | Region (Int,Int)

data Content = Bookmark | Highlight String | Annotation String

parseDate :: String -> LocalTime
parseDate = fst . fromMaybe (epoch,"") . strptime "%A, %d %B %y %X"
  where epoch = readTime defaultTimeLocale "%s" "0" :: LocalTime

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

emptyClipping :: Clipping
emptyClipping = Clipping emptyDocument emptyPosition emptyLocalTime Bookmark
  where emptyDocument  = Document [] Nothing
        emptyPosition  = Position Nothing Nothing
        emptyLocalTime = parseDate []

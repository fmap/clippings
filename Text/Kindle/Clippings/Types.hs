module Text.Kindle.Clippings.Types 
( Clipping (..)
, Document (..)
, Position (..)
, Location (..)
, Content  (..)
) where

import Data.Time.LocalTime (LocalTime)

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

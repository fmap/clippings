module Text.Kindle.Clippings.Types 
( Clipping (..)
, Document (..)
, Position (..)
, Interval (..)
, Content  (..)
) where

import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Data.Time.Calendar (fromGregorian)
import Data.Default

data Clipping = Clipping
  { document :: Document
  , position :: Position
  , date     :: LocalTime
  , content  :: Content
  } deriving (Eq)

data Document = Document 
  { title  :: String
  , author :: Maybe String -- Not always present, e.g. "Oxford Dictionary of English\n".
  } deriving (Eq)

data Interval = Singleton Int | Proper Int Int deriving (Eq)

data Position = Position
  { page     :: Maybe Interval
  , location :: Maybe Interval
  } deriving (Eq)

data Content = Bookmark | Highlight String | Annotation String

instance Eq Content where 
  Bookmark      == Bookmark       =  True
  Highlight s0  == Highlight s1   =  s0 == s1
  Annotation s0 == Annotation s1  =  s0 == s1
  _ == _                          =  False

instance Default Document where
  def = Document [] Nothing

instance Default Position where
  def = Position Nothing Nothing

instance Default LocalTime where
  def = LocalTime (fromGregorian 1970 1 1) (TimeOfDay 0 0 0)

instance Default Content where
  def = Bookmark

instance Default Clipping where
  def = Clipping def def def def

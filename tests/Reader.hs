{-# LANGUAGE StandaloneDeriving #-}

import Data.Monoid
import Control.Applicative 
import System.FilePath (splitFileName)
import System.Environment (getExecutablePath)
import Text.Kindle.Clippings.Types
import Text.Kindle.Clippings.Reader
import Text.Kindle.Clippings.Writer
import Text.Parsec (parse)
import Data.Maybe (fromMaybe)
import Data.Time.LocalTime
import Data.Time.Calendar
import System.Console.ANSI 

deriving instance Eq Clipping

deriving instance Eq Document

instance Eq Location where
  Location a == Location b = a==b
  Region a   == Region b = a==b
  _ ==  _ = False

deriving instance Eq Position

instance Eq Content where 
  Bookmark      == Bookmark       =  True
  Highlight s0  == Highlight s1   =  s0 == s1
  Annotation s0 == Annotation s1  =  s0 == s1
  _ == _                          =  False

class Default a where
  unit :: a

instance Default Clipping where
  unit = emptyClipping

fromMaybeEither :: Default b => Either a (Maybe b) -> b
fromMaybeEither = fromMaybe unit .$  either (Just . const unit) id
  
(.$) :: (b -> c) -> (a -> b) -> a -> c
(.$) = ((.) $)

getClipping :: String -> Clipping
getClipping a = fromMaybeEither $ parse readClipping "tests/Reader.hs" a

inFixture :: Clipping
inFixture = Clipping 
  { date     = LocalTime (fromGregorian 2013 06 10) (TimeOfDay 6 58 17)
  , document = Document "Haskell Monoids and their Uses" (Just "sigfpe")
  , position = Position Nothing . Just $ Region (3,4)
  , content  = Highlight "Haskell is a great language for constructing code modularly from small but orthogonal building blocks."
  }

assert :: String -> Bool -> IO ()
assert str prop = setSGR [SetColor Foreground Vivid col] >> putStrLn str >> setSGR []
  where col = if prop then Green else Red
  
main :: IO () 
main = do
  directory <- fst . splitFileName <$> getExecutablePath
  clipping  <- readFile $ directory <> "fixtures/clipping.txt"
  assert "Fixture should parse to sigfpe clipping." $ getClipping clipping == inFixture

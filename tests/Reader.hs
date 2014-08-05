import Control.Monad ((<=<))
import Data.Default (Default(def))
import Data.Maybe (fromMaybe)
import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Paths_clippings (getDataFileName)
import Test.Assert (runAssertions)
import Text.Kindle.Clippings.Reader (readClipping)
import Text.Kindle.Clippings.Types (Clipping(..),Location(..),Document(..),Position(..),Content(..))
import Text.Kindle.Clippings.Writer ()
import Text.Parsec (parse)

fromMaybeEither :: Default b => Either a (Maybe b) -> b
fromMaybeEither = fromMaybe def .$  either (Just . const def) id
  
(.$) :: (b -> c) -> (a -> b) -> a -> c
(.$) = ((.) $)

getClipping :: String -> Clipping
getClipping = fromMaybeEither . parse readClipping "tests/Reader.hs"

inFixture :: Clipping
inFixture = Clipping 
  { date     = LocalTime (fromGregorian 2013 06 10) (TimeOfDay 6 58 17)
  , document = Document "Haskell Monoids and their Uses" (Just "sigfpe")
  , position = Position Nothing . Just $ Region (3,4)
  , content  = Highlight "Haskell is a great language for constructing code modularly from small but orthogonal building blocks."
  }

inPw2Fixture :: Clipping
inPw2Fixture = Clipping
  { date     = LocalTime (fromGregorian 2014 06 08) (TimeOfDay 20 36 53)
  , document = Document "Stand on Zanzibar" (Just "John Brunner")
  , position = Position Nothing . Just $ Region (4607, 4607)
  , content  = Highlight "Shinka will"
  }

inPw2pdfFixture :: Clipping
inPw2pdfFixture = Clipping
  { date     = LocalTime (fromGregorian 2014 07 30) (TimeOfDay 14 02 57)
  , document = Document "Tyler Cowen-Creative Destruction_ How Globalization Is Changing the World's Cultures-Princeton University Press (2002)_k2opt" (Just "")
  , position = Position Nothing . Just $ Region (316, 316)
  , content  = Highlight "The fundamental story about consumer taste, in modern times, is not one of dumbing down or of producers seeking to satisfy a homogeneous least common denominator at the expense of quality. Rather, the basic trend is of increasing variety and diversity, at all levels of quality, high and low"
  }

getTitle :: Clipping -> String
getTitle = title . document

getAuthor :: Clipping -> Maybe String
getAuthor = author . document
  
main :: IO () 
main = do
  [clipping, brackets, nested, pw2, pw2pdf] <- mapM (readFile <=< getDataFileName)
    [ "tests/fixtures/clipping.txt"
    , "tests/fixtures/brackets.txt"
    , "tests/fixtures/nested_brackets.txt"
    , "tests/fixtures/pw2clipping.txt"
    , "tests/fixtures/pw2pdfclipping.txt"
    ]
  runAssertions $ 
    [ ("Fixture should parse to sigfpe clipping.", getClipping clipping == inFixture)
    , ("Brackets in clippings' titles should be preserved." , getTitle (getClipping brackets) == "An Introduction to Statistical Learning: with Applications in R (Springer Texts in Statistics)")
    , ("Nested brackets in clippings' authors should be preserved.", getAuthor (getClipping nested) == Just "G. K. (Gilbert Keith) Chesterton")
    , ("Pw2Fixture should parse to Zanzibar clipping", getClipping pw2 == inPw2Fixture)
    , ("Pw2pdfFixture should parse to Tyler clipping", getClipping pw2pdf == inPw2pdfFixture)
    ]

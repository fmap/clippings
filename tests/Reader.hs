import Data.Monoid ((<>))
import Control.Applicative  ((<$>))
import System.FilePath (splitFileName)
import System.Environment (getExecutablePath)
import Text.Kindle.Clippings.Types
import Text.Kindle.Clippings.Reader (readClipping)
import Text.Parsec (parse)
import Data.Maybe (fromMaybe)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Data.Time.Calendar (fromGregorian)
import Test.Assert (runAssertions)
import Data.Default
import Paths_clippings (getDataFileName)

fromMaybeEither :: Default b => Either a (Maybe b) -> b
fromMaybeEither = fromMaybe def .$  either (Just . const def) id
  
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

getTitle :: Clipping -> String
getTitle = title . document

getAuthor :: Clipping -> Maybe String
getAuthor = author . document
  
main :: IO () 
main = do
  clipping  <- readFile =<< getDataFileName "tests/fixtures/clipping.txt"
  brackets  <- readFile =<< getDataFileName "tests/fixtures/brackets.txt"
  nested    <- readFile =<< getDataFileName "tests/fixtures/nested_brackets.txt"
  runAssertions $ 
    [ ("Fixture should parse to sigfpe clipping.", getClipping clipping == inFixture)
    , ("Brackets in clippings' titles should be preserved." , getTitle (getClipping brackets) == "An Introduction to Statistical Learning: with Applications in R (Springer Texts in Statistics)")
    , ("Nested brackets in clippings' authors should be preserved.", getAuthor (getClipping nested) == Just "G. K. (Gilbert Keith) Chesterton")
    ]

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
import System.Console.ANSI 
import Data.Default

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

assert :: String -> Bool -> IO ()
assert str prop = setSGR [SetColor Foreground Dull col] >> putStrLn str >> setSGR []
  where col = if prop then Green else Red
  
main :: IO () 
main = do
  directory <- fst . splitFileName <$> getExecutablePath
  clipping  <- readFile $ directory <> "fixtures/clipping.txt"
  assert "Fixture should parse to sigfpe clipping." $ getClipping clipping == inFixture

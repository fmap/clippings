module Text.Kindle.Clippings.Reader where

import Control.Applicative ((<$>), (<*>), (*>), (<*), (<|>), many)
import Data.Char (isSpace)
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import Data.Time.LocalTime (LocalTime)
import Data.Time.Parse (strptime)
import Text.Kindle.Clippings.Types (Clipping(..),Location(..),Document(..),Position(..),Content(..))
import Text.Parsec (many1, digit, alphaNum, string, skipMany, oneOf, noneOf, try, char, manyTill, anyToken, optionMaybe)
import Text.Parsec.String (Parser)

eol :: Parser ()
eol = skipMany $ oneOf "\n\r"

eor :: Parser String
eor = string "=========="

chomp :: String -> String
chomp = rstrip . lstrip
  where lstrip = dropWhile isSpace
        rstrip = reverse . lstrip . reverse

but :: String -> Parser String
but = many . noneOf

tryBut1 :: String -> Parser String
tryBut1 = try . many1 . noneOf

readTitle :: Parser String
readTitle = chomp . concat <$> textAndBrackets
  where brackets = (\a b c -> a:(concat b)++c) <$> char '(' <*> textAndBrackets <*> string ") "
        textAndBrackets = many1 (tryBut1 "(\r\n)" <|> try brackets)

tryMaybe :: Parser a -> Parser (Maybe a)
tryMaybe = optionMaybe . try

($:) :: Functor f => (f a -> b) -> (c -> a) -> f c -> b
f0 $: f1 = fmap f0 (fmap f1)

readAuthor :: Parser (Maybe String)
readAuthor = tryMaybe $: init $ char '(' *> but "\n\r"

readContentType :: Parser String
readContentType = string "- " *> but " " <* string " "

readPageNumber :: Parser (Maybe Int)
readPageNumber = tryMaybe $: read $ string "on Page " *> many1 alphaNum <* string " | "

readLocation :: Parser (Maybe Location)
readLocation = tryMaybe 
             $ (try (string "Loc. ") <|> string "Location ")
            *> (try readLocationRegion <|> readLocationInt)
            <* but "|" <* string "| "

readLocationInt :: Parser Location
readLocationInt = Location . read <$> many1 digit

readLocationRegion :: Parser Location
readLocationRegion = toLocation <$> many1 digit <*> (char '-' *> many1 digit)
  where toLocation = parseRegion .: (,)

(.:) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(.:) = fmap fmap fmap

parseRegion :: (String, String) -> Location
parseRegion (s0,s1) = Region . readTuple $ pad (s0,s1)
  where readTuple (s2,s3) = (read s2, read s3) 

pad :: (String,String) -> (String,String)
pad (s0,s1) = (s0, pr++s1)
  where pr = take (length s0 - length s1) s0

parseDate :: String -> LocalTime
parseDate = fst . fromJust . fromJust {-[^1]-} . find isJust . flip map formats . flip strptime
  where formats =
          [ "%A, %d %B %y %X"
          , "%A, %B %d, %Y %r"
          , ""
          ]
-- [^1]: This is safe: `strptime x ""` is `Just` for all `x`.

readDate :: Parser LocalTime
readDate = fmap parseDate $ string "Added on " *> but "\n\r"

readContent :: Parser String
readContent = fmap chomp . manyTill anyToken $ try eor

readClipping :: Parser (Maybe Clipping)
readClipping = clipping
           <$> (Document <$> readTitle <*> readAuthor <* eol)
           <*> readContentType
           <*> (Position <$> readPageNumber <*> readLocation)
           <*> readDate <* eol
           <*> readContent <* eol

clipping :: Document -> String -> Position -> LocalTime -> String -> Maybe Clipping
clipping d t p l c
  |(==) t "Highlight" = Just $ Clipping d p l $ Highlight c
  |(==) t "Note"      = Just $ Clipping d p l $ Annotation c
  |(==) t "Bookmark"  = Just $ Clipping d p l Bookmark
  | otherwise = Nothing

readClippings :: Parser [Maybe Clipping]
readClippings = many1 readClipping

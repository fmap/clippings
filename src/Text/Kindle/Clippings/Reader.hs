module Text.Kindle.Clippings.Reader where

import Text.Parsec hiding ((<|>), many)
import Text.Parsec.String
import Data.Char (isSpace)
import Data.Time.LocalTime (LocalTime)
import Text.Kindle.Clippings.Types 
import Data.Default
import Control.Applicative 
import Data.Time.LocalTime (LocalTime)
import Data.Time.Parse (strptime)
import Data.Maybe (fromMaybe)

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
readTitle = chomp . concat <$> many1 (tryBut1 "(\r\n)" <|> try brackets)
  where brackets = (\a b c -> a:b++c) <$> char '(' <*> but "()" <*> string ") "

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
readLocation = tryMaybe $ string "Loc. " *> readLocation' <* but "|" <* string "| "

readLocation' :: Parser Location
readLocation' = (try readLocationRegion) <|> readLocationInt

readLocationInt :: Parser Location
readLocationInt = Location . read <$> many1 digit

(.:) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(.:) = fmap fmap fmap

readLocationRegion :: Parser Location
readLocationRegion = toLocation <$> many1 digit <*> (char '-' *> many1 digit)
  where toLocation = parseRegion .: (,)

parseRegion :: (String, String) -> Location
parseRegion (s0,s1) = Region . readTuple $ pad (s0,s1)
  where readTuple (s2,s3) = (read s2, read s3) 

pad :: (String,String) -> (String,String)
pad (s0,s1) = (s0, pr++s1)
  where pr = take (length s0 - length s1) s0

parseDate :: String -> LocalTime
parseDate = fromMaybe def . fmap fst . strptime "%A, %d %B %y %X"

readDate :: Parser LocalTime
readDate = fmap parseDate $ string "Added on " *> but "\n\r"

readContent :: Parser String
readContent = do
  content <- manyTill anyToken $ try $ string "=========="
  return $ chomp content

readClipping :: Parser (Maybe Clipping)
readClipping = do
  title  <- readTitle
  author <- readAuthor
  eol
  typ  <- readContentType
  page <- readPageNumber
  loc  <- readLocation
  date <- readDate
  eol
  content <- readContent
  eol
  return $ clipping typ (Document title author) (Position page loc) date content

clipping :: String -> Document -> Position -> LocalTime -> String -> Maybe Clipping
clipping t d p l c
  |(==) t "Highlight" = Just $ Clipping d p l $ Highlight c
  |(==) t "Note"      = Just $ Clipping d p l $ Annotation c
  |(==) t "Bookmark"  = Just $ Clipping d p l Bookmark
  | otherwise = Nothing

readClippings :: Parser [Maybe Clipping]
readClippings = many1 readClipping

module Text.Kindle.Clippings.Reader where

import Text.Parsec hiding ((<|>), many)
import Text.Parsec.String
import Data.Char (isSpace)
import Data.Time.LocalTime (LocalTime)
import Text.Kindle.Clippings.Types 
import Text.Kindle.Clippings.Writer (emptyClipping, parseDate)
import Control.Applicative 

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

readTitle :: Parser String
readTitle = chomp <$> but "(\n\r"

tryMaybe :: Parser a -> Parser (Maybe a)
tryMaybe = optionMaybe . try

readAuthor :: Parser (Maybe String)
readAuthor = tryMaybe <$> fmap init $ char '(' *> but "\n\r"

readContentType :: Parser String
readContentType = string "- " *> but " " <* string " "

readPageNumber :: Parser (Maybe Int)
readPageNumber = tryMaybe <$> fmap read $ string "on Page " *> many1 alphaNum <* string " | "

readLocation :: Parser (Maybe Location)
readLocation = tryMaybe $ string "Loc. " *> readLocation' <* but "|" <* string "| "

readLocation' :: Parser Location
readLocation' = (try readLocationRegion) <|> readLocationInt

readLocationInt :: Parser Location
readLocationInt = Location . read <$> many1 digit

readLocationRegion :: Parser Location
readLocationRegion = toLocation <$> many1 digit <*> (char '-' *> many1 digit)
  where toLocation = (parseRegion .) . (,)

parseRegion :: (String, String) -> Location
parseRegion (s0,s1) = Region . readTuple $ pad (s0,s1)
  where readTuple (s2,s3) = (read s2, read s3) 

pad :: (String,String) -> (String,String)
pad (s0,s1) = (s0, pr++s1)
  where pr = take (length s0 - length s1) s0

readDate :: Parser LocalTime
readDate = fmap parseDate $ string "Added on" *> but "\n\r"

readContent :: Parser String
readContent = fmap chomp $ manyTill anyToken . try $ string "=========="

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

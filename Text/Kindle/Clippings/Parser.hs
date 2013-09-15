module Text.Kindle.Clippings.Parser where

import Text.Kindle.Clippings.Types
import Prelude hiding (until)
import Text.Parsec
import Text.Parsec.String
import Data.Char (isSpace)
import Data.Time.LocalTime (LocalTime)

eol :: Parser ()
eol = skipMany $ oneOf "\n\r"

eor :: Parser String
eor = string "=========="

until :: String -> Parser String
until e = many $ noneOf e

chomp :: String -> String
chomp = reverse . dropWhile isSpace . reverse . dropWhile isSpace  --lol

readTitle :: Parser String
readTitle = do
  title <- many $ noneOf "(\n\r"
  return $ chomp title

inBrackets :: Parser String
inBrackets = do
  char '('
  author <- many $ noneOf ")"
  char ')'
  return author

readAuthor :: Parser (Maybe String)
readAuthor = optionMaybe $ try $ inBrackets

readContentType :: Parser String
readContentType = do
  string "- "
  contentType <- until " "
  string " "
  return contentType

readPageNumber :: Parser (Maybe Int)
readPageNumber = optionMaybe $ try $ do
  string "on Page "
  number <- many1 alphaNum --roman numerals lol
  string " | "
  return $ read number

readLocation :: Parser (Maybe Location)
readLocation = optionMaybe $ try $ do
  string "Loc. "
  location <- readLocation'
  until "|"
  string "| "
  return location

readLocation' :: Parser Location
readLocation' = (try readLocationRegion) <|> readLocationInt

readLocationInt :: Parser Location
readLocationInt = do
  s0 <- many1 digit
  return $ Location (read s0 ::Int)

readLocationRegion :: Parser Location
readLocationRegion = do
  s0 <- many1 digit 
  char '-'
  s1 <- many1 digit
  return $ parseRegion (s0,s1)

parseRegion :: (String, String) -> Location
parseRegion (s0,s1) = Region $ readTuple $ pad (s0,s1)
  where readTuple (s2,s3) = (read s2, read s3) 

pad :: (String,String) -> (String,String)
pad (s0,s1) = (s0,pr++s1)
  where pr = take (length s0 - length s1) s0

readDate :: Parser LocalTime
readDate = do
  string "Added on "
  date <- until "\n\r"
  return $ parseDate date

readContent :: Parser String
readContent = do
  content <- manyTill anyToken $ try $ string "=========="
  return $ chomp content

readClipping :: Parser Clipping
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

clipping :: String -> Document -> Position -> LocalTime -> String -> Clipping
clipping t d p l c
  |(==) t "Highlight" = Clipping d p l $ Highlight c
  |(==) t "Note"      = Clipping d p l $ Annotation c
  |(==) t "Bookmark"  = Clipping d p l Bookmark
  | otherwise = emptyClipping -- *something went wrong*

readClippings :: Parser [Clipping]
readClippings = do
  many1 readClipping

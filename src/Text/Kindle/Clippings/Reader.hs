module Text.Kindle.Clippings.Reader
( readClipping
, readClippings
) where

import Control.Applicative ((<$>), (<*>), (*>), (<*), (<|>), liftA2)
import Control.Monad (join)
import Data.Functor.Infix ((<$$>))
import Data.List (find)
import Data.Maybe (isJust, fromMaybe)
import Data.Time.LocalTime (LocalTime)
import Data.Time.Parse (strptime)
import Text.Kindle.Clippings.Types (Clipping(..),Interval(..),Document(..),Position(..),Content(..))
import Text.Parsec (many1, digit, string, oneOf, try, char, manyTill, anyToken, lookAhead, many, noneOf, between)
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator.Extras (optional, but, tryString, stringCI)

data Tree = Leaf String | Node [Tree]

brackets :: Parser Tree
brackets = Node <$> between
  (char '(') (char ')')
  (many (brackets <|> (Leaf <$> many1 (noneOf "()"))))

instance Show Tree where
  show (Leaf x) = x
  show (Node xs) = "(" ++ concatMap show xs ++ ")"

node :: a -> ([Tree] -> a) -> (Tree -> a)
node _ fun (Node xs) = fun xs
node def _ _ = def

-- N.B.
-- The document parser (i.e. 'author' + 'title') is known to fail where
-- the author component includes unmatched parentheses, however this case
-- appears ambiguous in the grammar.

space :: Parser String
space = string " "

eol :: Parser String
eol = many1 $ oneOf "\n\r"

readTitle :: Parser String
readTitle = manyTill anyToken (lookAhead . try $ space *> (try brackets <|> (Leaf <$> space)) *> eol)

readAuthor :: Parser String
readAuthor = node (error "The impossible happened!") (concatMap show) <$> brackets

readContentType :: Parser String
readContentType = (tryString "- Your " <|> string "- ")
               *> but " "
               <* (tryString " on " <|> tryString " at " <|> many1 (char ' '))

parseSingletonInterval :: Parser Interval
parseSingletonInterval = Singleton . read <$> many1 digit

-- Early Kindle models sometimes described intervals of locations
-- with the prefix of the second part removed; e.g. ("1109", "12").
-- this padding will normalise this to (1109, 1112).
normaliseRegion :: String -> String -> (Int, Int)
normaliseRegion s0 s1 = (read s0, read $ take (length s0 - length s1) s0++s1)

parseProperInterval :: Parser Interval
parseProperInterval = (uncurry Proper <$$> normaliseRegion) <$> many1 digit <*> (char '-' *> many1 digit)

parseInterval :: Parser Interval
parseInterval = try parseProperInterval <|> parseSingletonInterval

readPage :: Parser Interval
readPage = stringCI "Page " *> parseInterval <* string " | "

readLocation :: Parser Interval
readLocation = (tryString "Loc. " <|> stringCI "Location ")
            *> parseInterval
            <* many1 (oneOf " |")

parseDate :: String -> LocalTime
parseDate raw = fromMaybe defaultLocalTime . join . find isJust . map (fst <$$> flip strptime raw) $
  [ "%A, %d %B %y %X"  -- Thursday, 01 January 70 12:00:00 AM
  , "%A, %B %d, %Y %r" -- Thursday, January 01, 1970 12:00:00 AM
  ]

defaultLocalTime :: LocalTime
Just (defaultLocalTime, _) = strptime "" ""

readDate :: Parser LocalTime
readDate = string "Added on " *> (parseDate <$> but "\n\r")

eor :: Parser String
eor = string "=========="

readContent :: Parser String
readContent = manyTill anyToken (try $ many1 (oneOf "\n\r ") *> eor)

readClipping :: Parser (Maybe Clipping)
readClipping = clipping
           <$> liftA2 Document readTitle (many1 space *> optional readAuthor) <* eol
           <*> readContentType
           <*> liftA2 Position (optional readPage) (optional readLocation)
           <*> readDate <* eol
           <*> readContent <* eol

clipping :: Document -> String -> Position -> LocalTime -> String -> Maybe Clipping
clipping d t p l c
  | (==) t "Highlight" = Just . Clipping d p l $ Highlight c
  | (==) t "Note"      = Just . Clipping d p l $ Annotation c
  | (==) t "Bookmark"  = Just . Clipping d p l $ Bookmark
  | otherwise = Nothing

readClippings :: Parser [Maybe Clipping]
readClippings = many1 readClipping

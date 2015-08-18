module Text.Parsec.Combinator.Extras (
  but,
  tryBut1,
  tryMaybe,
  tryString,
  stringCI
) where

import Control.Applicative ((<|>))
import Data.Char (toLower, toUpper)
import Text.Parsec.String (Parser)
import Text.Parsec (many, many1, noneOf, try, optionMaybe, string, char)

but :: String -> Parser String
but = many . noneOf

tryBut1 :: String -> Parser String
tryBut1 = try . many1 . noneOf

tryMaybe :: Parser a -> Parser (Maybe a)
tryMaybe = optionMaybe . try

tryString :: String -> Parser String
tryString = try . string

stringCI :: String -> Parser String
stringCI = mapM (\c -> char (toUpper c) <|> char (toLower c))

module Text.Parsec.Combinator.Extras (
  optional,
  but,
  tryString,
  stringCI
) where

import Control.Applicative ((<|>))
import Data.Char (toLower, toUpper)
import Text.Parsec.String (Parser)
import Text.Parsec (many, noneOf, try, optionMaybe, string, char)

optional :: Parser a -> Parser (Maybe a)
optional = optionMaybe . try

but :: String -> Parser String
but = many . noneOf

tryString :: String -> Parser String
tryString = try . string

stringCI :: String -> Parser String
stringCI = mapM (\c -> char (toUpper c) <|> char (toLower c))

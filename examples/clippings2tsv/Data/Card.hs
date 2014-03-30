module Data.Card (Card(..), ToCard(..)) where

import Data.Csv (ToRecord(..), toField, record)

data Card = Card { question :: String, answer :: String }

instance ToRecord Card where
  toRecord (Card q a) = record $ map toField [q,a]

class ToCard a where
  toCard :: a -> Maybe Card


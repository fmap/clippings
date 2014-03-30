module Data.Either.Extras (bimapEither) where

bimapEither :: (a -> b) -> (c -> d) -> Either a c -> Either b d
bimapEither f g either = case either of 
  Left  left  -> Left  $ f left
  Right right -> Right $ g right

module Data.List.Extras (substitute) where

substitute :: Eq a => a -> a -> [a] -> [a]
substitute term replacement = map (\x -> if x == term then replacement else x)

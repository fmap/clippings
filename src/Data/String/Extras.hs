module Data.String.Extras (pad, chomp) where

import Data.Char (isSpace)

pad :: (String,String) -> (String,String)
pad (s0,s1) = (s0, pr++s1)
  where pr = take (length s0 - length s1) s0

chomp :: String -> String
chomp = rstrip . lstrip
  where lstrip = dropWhile isSpace
        rstrip = reverse . lstrip . reverse

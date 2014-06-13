module Data.Csv.Extras (encodeTabDelimited) where

import Data.Char (ord)
import Data.Csv (ToRecord(..), EncodeOptions(..), encodeWith, defaultEncodeOptions)
import Data.ByteString.Lazy (ByteString)

encodeTabDelimited :: ToRecord a => [a] -> ByteString
encodeTabDelimited = encodeWith tabDelimited

tabDelimited :: EncodeOptions
tabDelimited = defaultEncodeOptions
  { encDelimiter = fromIntegral $ ord '\t'
  }

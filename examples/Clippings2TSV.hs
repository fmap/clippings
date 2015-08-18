{-# LANGUAGE RecordWildCards, LambdaCase #-}

module Main where

import Prelude hiding (putStr)
import Control.Applicative ((<$>))
import Data.Bifunctor (Bifunctor(bimap))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (putStr)
import Data.Char (ord)
import Data.Csv (ToRecord(..), EncodeOptions(..), encodeWith, defaultEncodeOptions, toField, record)
import Data.Functor.Infix ((<$$>))
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid ((<>))
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStr, stderr)
import Text.Kindle.Clippings (Clipping(..), Document(..), Content(..), readClippings)
import Text.Parsec (parse)

data Card = Card { question :: String, answer :: String }

instance ToRecord Card where
  toRecord (Card q a) = record $ map toField [q,a]

class ToCard a where
  toCard :: a -> Maybe Card

instance ToCard Clipping where
  toCard c@Clipping{..} 
    | not (isHighlight c) = Nothing
    | null (show content) = Nothing
    | otherwise = Just $ Card content' author'
    where author'  = fromMaybe "[clippings2tsv]" $ (" - " <>) <$> author document
          content' = \case { '\n' -> ' '; chr -> chr; } <$> show content

isHighlight :: Clipping -> Bool
isHighlight Clipping{..} = case content of
  Highlight _ -> True
  _           -> False

getClippings :: String -> Either String [Clipping]
getClippings = bimap show catMaybes 
             . parse readClippings [] 

encodeTabDelimited :: ToRecord a => [a] -> ByteString
encodeTabDelimited = encodeWith $ defaultEncodeOptions
  { encDelimiter = fromIntegral $ ord '\t' }

renderClippings :: [Clipping] -> ByteString
renderClippings = encodeTabDelimited . catMaybes . fmap toCard

main :: IO ()
main = head <$> getArgs >>= getClippings <$$> readFile >>= \case
  Left err -> hPutStr stderr err >> exitFailure
  Right cs -> putStr (renderClippings cs) >> exitSuccess

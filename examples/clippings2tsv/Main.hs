{-# LANGUAGE RecordWildCards, LambdaCase #-}

module Main where

import Prelude hiding (putStr)
import Control.Applicative ((<$>))
import Control.Applicative.Extras ((<$$>))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (putStr)
import Data.Card (Card(..), ToCard(..))
import Data.Csv.Extras (encodeTabDelimited) 
import Data.Either.Extras (bimapEither)
import Data.List.Extras (substitute)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid ((<>), Monoid (mempty))
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStr, stderr)
import Text.Kindle.Clippings (Clipping(..), Document(..), Content(..), readClippings)
import Text.Parsec (parse)

instance ToCard Clipping where
  toCard c@Clipping{..} 
    | not (isHighlight c) = Nothing
    | null (show content) = Nothing
    | otherwise = Just $ Card content' author'
    where author'  = fromMaybe "[clippings2tsv]" $ (" - " <>) <$> author document
          content' = substitute '\n' ' ' $ show content

isHighlight :: Clipping -> Bool
isHighlight Clipping{..} = case content of
  Highlight _ -> True
  _           -> False

getClippings :: String -> Either String [Clipping]
getClippings = bimapEither show catMaybes 
             . parse readClippings [] 

renderClippings :: [Clipping] -> ByteString
renderClippings = encodeTabDelimited . catMaybes . fmap toCard

main :: IO ()
main = head <$> getArgs >>= getClippings <$$> readFile >>= \case
  Left err -> hPutStr stderr err >> exitFailure
  Right cs -> putStr (renderClippings cs) >> exitSuccess

{-# LANGUAGE RecordWildCards, LambdaCase #-}

module Main where

import Control.Applicative ((<$>))
import Control.Applicative.Extras ((<$$>))
import Data.Either.Extras (bimapEither)
import Data.List.Extras (substitute)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid ((<>), Monoid (mempty))
import Data.Markdown.Extras
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStr, stderr)
import Text.Kindle.Clippings (Clipping(..), Document(..), Content(..), readClippings)
import Text.Parsec (parse)

getClippings :: String -> Either String [Clipping]
getClippings = bimapEither show catMaybes 
             . parse readClippings [] 

renderClippings :: [Clipping] -> [String]
renderClippings = fmap encodeMarkdown

main :: IO ()
main = head <$> getArgs >>= getClippings <$$> readFile >>= \case
  Left err -> hPutStr stderr err >> exitFailure
  Right cs -> putStr (head $ renderClippings cs) >> exitSuccess

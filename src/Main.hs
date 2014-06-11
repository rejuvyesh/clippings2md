{-# LANGUAGE RecordWildCards, LambdaCase #-}

module Main where

import Control.Applicative ( (<$>) )
import Control.Applicative.Extras ( (<$$>) )
import Data.Either.Extras ( bimapEither )
import Data.List.Extras ()
import Data.Maybe ( catMaybes )
import Data.Monoid ( Monoid(mconcat) )
import Data.Markdown.Extras ( encodeMarkdown )
import System.Environment ( getArgs )
import System.Exit ( exitSuccess, exitFailure )
import System.IO ( hPutStr, stderr )
import Text.Kindle.Clippings ( Clipping, readClippings )
import Text.Parsec ( parse )
import Text.Pandoc ( def, Pandoc(Pandoc), Block, writeMarkdown )
import Text.Pandoc.Builder as TPB ( toList )

getClippings :: String -> Either String [Clipping]
getClippings = bimapEither show catMaybes 
             . parse readClippings [] 

renderClippings :: [Clipping] -> [Block]
renderClippings = mconcat . fmap TPB.toList . catMaybes . fmap encodeMarkdown

main :: IO ()
main = head <$> getArgs >>= getClippings <$$> readFile >>= \case
  Left err -> hPutStr stderr err >> exitFailure
  Right cs -> putStrLn (writeMarkdown def (Pandoc undefined (renderClippings cs))) >> exitSuccess

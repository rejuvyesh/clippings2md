{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Applicative   ((<$>))
import           Data.Bifunctor        (Bifunctor (bimap))
import           Data.Functor.Extras   ((<$$>))
import           Data.Maybe            (catMaybes)
import           Data.Monoid           (Monoid (mconcat))
import           System.Environment    (getArgs)
import           System.Exit           (exitFailure, exitSuccess)
import           System.IO             (hPutStr, stderr)
import           Text.Kindle.Clippings (Clipping, readClippings)
import           Text.Markdown.Extras  (encodeMarkdown)
import           Text.Pandoc           (Block, Pandoc (Pandoc), def,
                                        writeMarkdown)
import           Text.Pandoc.Builder   as TPB (toList)
import           Text.Parsec           (parse)

getClippings :: String -> Either String [Clipping]
getClippings = bimap show catMaybes . parse readClippings []

renderClippings :: [Clipping] -> [Block]
renderClippings = mconcat
                  . fmap TPB.toList
                  . catMaybes
                  . fmap encodeMarkdown
                  . reverse

main :: IO ()
main = head <$> getArgs >>= getClippings <$$> readFile >>= \case
  Left err -> hPutStr stderr err >> exitFailure
  Right cs -> putStrLn (writeMarkdown def (Pandoc undefined (renderClippings cs))) >> exitSuccess

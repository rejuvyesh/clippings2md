{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Text.Markdown.Extras (encodeMarkdown) where

import           Control.Applicative         ((<$>))
import           Data.Maybe                  (fromJust, fromMaybe, isNothing)
import           Text.Kindle.Clippings.Types (Clipping (content, document),
                                              Content (Highlight),
                                              Document (author, title))
import           Text.Pandoc.Builder         as TPB (Blocks, blockQuote, header,
                                                     para, text, (<>))

getTitle :: Clipping -> String
getTitle = title . document

getAuthor :: Clipping -> Maybe String
getAuthor = author . document

getHighlightFromContent :: Content -> Maybe String
getHighlightFromContent (Highlight str) = Just str
getHighlightFromContent _ = Nothing

getHighlight :: Clipping -> Maybe String
getHighlight = getHighlightFromContent . content

encodeMarkdown :: Clipping -> Maybe Blocks
encodeMarkdown c
  | isNothing $ highlight = Nothing
  | null (fromJust highlight) = Nothing
  | otherwise = Just ((header 3 $ text (getTitle c)) <>
                (blockQuote $ para $ text $ (fromJust highlight)) <>
                (para $ text $ (fromMaybe "[clippings2md]" $ (" --- "<>) <$> getAuthor c)))
  where highlight = getHighlight c


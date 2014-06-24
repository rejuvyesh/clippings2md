{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Text.Markdown.Extras (encodeMarkdown) where

import Control.Applicative ( (<$>) )
import Data.Maybe ( fromMaybe, isNothing, fromJust )
import Text.Kindle.Clippings.Types
  ( Clipping(content, document),
    Document(author, title),
    Content(Highlight) )
import Text.Pandoc.Builder as TPB
  ( Blocks, (<>), text, para, header )

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
                (para $ text $ (fromJust highlight)) <>
                (para $ text $ (fromMaybe "[clippings2md]" $ (" - "<>) <$> getAuthor c)))
  where highlight = getHighlight c
        

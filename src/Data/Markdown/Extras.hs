{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Data.Markdown.Extras (encodeMarkdown) where

import Control.Applicative ((<$>))
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Text.Kindle.Clippings.Types
import Data.List.Extras (substitute)
import Text.Pandoc
import Text.Pandoc.Builder as TPB

getTitle :: Clipping -> String
getTitle = title . document

getAuthor :: Clipping -> Maybe String
getAuthor = author . document

-- getContent :: Clipping -> String
-- getContent = show content

isHighlight :: Clipping -> Bool
isHighlight Clipping{..} = case content of
  Highlight _ -> True
  _           -> False

encodeMarkdown :: Clipping -> Blocks
encodeMarkdown c = (header 3 $ text (getTitle c)) <> (para $ text $ (fromMaybe "[clippings2md]" $ (" - "<>) <$> getAuthor c))
                     -- -- Plain [Str (fromMaybe "[clippings2md]" $ (" - "<>) <$> getAuthor c)]
                     -- Plain [Str (getContent c)]
                   -- ]

        -- content' = substitute '\n' ' ' $ show $ getContent c
        

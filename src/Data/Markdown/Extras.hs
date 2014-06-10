module Data.Markdown.Extras (encodeMarkdown) where

import Control.Applicative ((<$>))
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Text.Kindle.Clippings.Types
import Data.List.Extras (substitute)

getTitle :: Clipping -> String
getTitle = title . document

getAuthor :: Clipping -> Maybe String
getAuthor = author . document

getContent :: Clipping -> Content
getContent = content

isHighlight :: Clipping -> Bool
isHighlight Clipping{..} = case content of
  Highlight _ -> True
  _           -> False

encodeMarkdown :: Clipping -> String
encodeMarkdown c = "### " ++ title' ++ "\n\n" ++ "\n" ++ author'
  where title'   = getTitle c
        author'  = fromMaybe "[clippings2md]" $ (" - "<>) <$> getAuthor c
        -- content' = substitute '\n' ' ' $ show $ getContent c
        

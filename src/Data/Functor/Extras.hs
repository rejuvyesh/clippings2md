module Data.Functor.Extras (
  (<$$>),
) where

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap fmap fmap

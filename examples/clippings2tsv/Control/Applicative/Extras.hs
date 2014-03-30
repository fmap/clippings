module Control.Applicative.Extras ((<$$>)) where

(<$$>) :: (Functor g, Functor f) => (a -> b) -> g (f a) -> g (f b)
(<$$>) = fmap . fmap


{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Conduit.Class where

class IsConduit conduit where
    yield :: Monad m => o -> conduit i o m ()
    await :: Monad m => conduit i o m (Maybe i)
    fuse :: Monad m => conduit a b m () -> conduit b c m r -> conduit a c m r

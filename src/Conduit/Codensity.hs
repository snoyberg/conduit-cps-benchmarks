{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Conduit.Codensity (run) where

import Conduit.Class
import Control.Monad (liftM, (>=>), ap)
import Control.Applicative (Applicative (pure, (<*>)))
import Control.Monad.Trans.Class (MonadTrans, lift)

data Step i o m r
    = Done r
    | M (m (Step i o m r))
    | Await (Step i o m r) (i -> Step i o m r)
    | Yield o (Step i o m r)

newtype Conduit i o m r = Conduit
    { unConduit :: forall b. (r -> Step i o m b) -> Step i o m b
    }

instance Monad m => Monad (Conduit i o m) where
    return r = Conduit ($ r)
    {-# INLINE return #-}

    Conduit f >>= g = Conduit $ \h -> f $ \x -> unConduit (g x) h
    {-# INLINE (>>=) #-}

instance Monad m => Functor (Conduit i o m) where
    fmap = liftM
instance Monad m => Applicative (Conduit i o m) where
    pure = return
    (<*>) = ap

instance MonadTrans (Conduit i o) where
    lift mr = Conduit $ \rest -> M (liftM rest mr)

instance IsConduit Conduit where
    yield o = Conduit $ \rest -> Yield o (rest ())
    {-# INLINE yield #-}

    await = Conduit $ \rest -> Await (rest Nothing) (rest . Just)
    {-# INLINE await #-}

    fuse (Conduit left0) (Conduit right0) = Conduit $ \rest -> let
        goR _left (Done r) = rest r
        goR left (M mc) = M (liftM (goR left) mc)
        goR left (Await x y) = goL x y left
        goR left (Yield o c) = Yield o (goR left c)

        goL x _y (Done ()) = goR (Done ()) x
        goL _x y (Yield o c) = goR c (y o)
        goL x y (M mc) = M (liftM (goL x y) mc)
        goL x y (Await x' y') = Await
            (goL x y x')
            (\a -> goL x y (y' a))

        in goR (left0 Done) (right0 Done)
    {-# INLINE fuse #-}

run :: Monad m => Conduit i o m r -> m r
run (Conduit c0) =
    go (c0 Done)
  where
    go (Done r) = return r
    go (M mc) = mc >>= go
    go (Yield _ c) = go c
    go (Await c _) = go c
{-# INLINE run #-}

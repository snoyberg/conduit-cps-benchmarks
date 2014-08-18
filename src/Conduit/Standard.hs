{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Conduit.Standard (run) where

import Conduit.Class
import Control.Monad (liftM, (>=>), ap)
import Control.Applicative (Applicative (pure, (<*>)))
import Control.Monad.Trans.Class (MonadTrans, lift)

data Conduit i o m r
    = Done r
    | M (m (Conduit i o m r))
    | Await (Conduit i o m r) (i -> Conduit i o m r)
    | Yield o (Conduit i o m r)

instance Monad m => Monad (Conduit i o m) where
    return = Done
    {-# INLINE return #-}

    Done r >>= f = f r
    M mc >>= f = M (liftM (>>= f) mc)
    Await x y >>= f = Await (x >>= f) (y >=> f)
    Yield o c >>= f = Yield o (c >>= f)
    {-# INLINE (>>=) #-}

instance Monad m => Functor (Conduit i o m) where
    fmap = liftM
instance Monad m => Applicative (Conduit i o m) where
    pure = return
    (<*>) = ap

instance MonadTrans (Conduit i o) where
    lift mr = M (liftM Done mr)

instance IsConduit Conduit where
    yield o = Yield o (Done ())
    {-# INLINE yield #-}

    await = Await (Done Nothing) (Done . Just)
    {-# INLINE await #-}

    fuse =
        goR
      where
        goR _left (Done r) = Done r
        goR left (M mc) = M (liftM (goR left) mc)
        goR left (Await x y) = goL x y left
        goR left (Yield o c) = Yield o (goR left c)

        goL x _y (Done ()) = goR (Done ()) x
        goL _x y (Yield o c) = goR c (y o)
        goL x y (M mc) = M (liftM (goL x y) mc)
        goL x y (Await x' y') = Await
            (goL x y x')
            (\a -> goL x y (y' a))
    {-# INLINE fuse #-}

run :: Monad m => Conduit i o m r -> m r
run =
    go
  where
    go (Done r) = return r
    go (M mc) = mc >>= go
    go (Yield _ c) = go c
    go (Await c _) = go c
{-# INLINE run #-}

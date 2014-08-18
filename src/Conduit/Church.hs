{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Conduit.Church (run) where

import Conduit.Class
import Control.Monad (liftM, (>=>), ap)
import Control.Applicative (Applicative (pure, (<*>)))
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Free.Church
import Control.Monad.Trans.Free (FreeT (..), FreeF (..))

data Step i o s
    = Await s (i -> s)
    | Yield o s
    deriving Functor

newtype Conduit i o m r = Conduit { unConduit :: FT (Step i o) m r }
    deriving (Monad, Functor, Applicative, MonadTrans)

instance IsConduit Conduit where
    yield o = Conduit $ FT $ \kp kf -> kf $ Yield o $ kp ()
    {-# INLINE yield #-}

    await = Conduit $ FT $ \kp kf -> kf $ Await (kp Nothing) (kp . Just)
    {-# INLINE await #-}

    fuse (Conduit left0) (Conduit right0) =
        Conduit $ toFT $ FreeT $ goR (fromFT left0) (fromFT right0)
      where
        goR left (FreeT mright) = do
            right <- mright
            case right of
                Pure r -> return $ Pure r
                Free (Yield o right') -> return $ Free $ Yield o $ FreeT $ goR left right'
                Free (Await x y) -> goL x y left

        goL x y (FreeT mleft) = do
            left <- mleft
            case left of
                Pure () -> goR (FreeT $ return $ Pure ()) x
                Free (Yield o left') -> goR left' (y o)
                Free (Await x' y') -> return $ Free $ Await
                    (FreeT $ goL x y x')
                    (\i -> FreeT $ goL x y (y' i))

run :: Monad m => Conduit i o m r -> m r
run =
    iterT go . unConduit
  where
    go (Yield _ f) = f
    go (Await f _) = f
{-# INLINE run #-}
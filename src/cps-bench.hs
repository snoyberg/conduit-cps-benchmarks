{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
import Test.Hspec
import Test.Hspec.QuickCheck
import System.Environment (withArgs)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Criterion.Main
import Data.Functor.Identity (runIdentity)

import Conduit.Class
import qualified Conduit.Standard
import qualified Conduit.Free
import qualified Conduit.Codensity
import Data.Word (Word8)
import qualified Data.Map as Map
import Data.IORef
import System.IO.Unsafe

enumFromToC :: (IsConduit c, Monad m, Enum a, Eq a, Monad (c i a m)) => a -> a -> c i a m ()
enumFromToC x0 y =
    loop x0
  where
    loop x
        | x == y = yield x
        | otherwise = yield x >> loop (succ x)
{-# INLINE enumFromToC #-}

foldC :: (IsConduit c, Monad m, Monad (c a o m)) => (b -> a -> b) -> b -> c a o m b
foldC f =
    loop
  where
    loop !b = await >>= maybe (return b) (loop . f b)
{-# INLINE foldC #-}

foldMC :: (IsConduit c, Monad m, Monad (c a o m), MonadTrans (c a o))
       => (b -> a -> m b) -> b -> c a o m b
foldMC f =
    loop
  where
    loop !b = await >>= maybe (return b) (\a -> lift (f b a) >>= loop)
{-# INLINE foldMC #-}

mapC f =
    loop
  where
    loop = await >>= maybe (return ()) (\a -> yield (f a) >> loop)

plusM :: (Num a, Monad m) => a -> a -> m a
plusM x y = return $! x + y

toSpec name run run2 = describe name $ do
    prop "mapM_ yield, pure sum" $ \is -> do
        let expected = sum is :: Int
        actual <- run $ mapM_ yield is `fuse` foldC (+) 0
        actual `shouldBe` expected
    prop "mapM_ yield, monadic sum" $ \is -> do
        let expected = sum is :: Int
        actual <- run $ mapM_ yield is `fuse` foldMC plusM 0
        actual `shouldBe` expected
    prop "enumFromTo, pure sum" $ \upper -> do
        let expected = sum [0..upper] :: Word8
        actual <- run2 $ enumFromToC 0 upper `fuse` foldC (+) 0
        actual `shouldBe` expected
    prop "enumFromTo, monadic sum" $ \upper -> do
        let expected = sum [0..upper] :: Word8
        actual <- run2 $ enumFromToC 0 upper `fuse` foldMC plusM 0
        actual `shouldBe` expected
    prop "mapM_ yield, map, pure sum" $ \is -> do
        let expected = sum $ map (+ 1) is :: Int
        actual <- run $ mapM_ yield is `fuse` mapC (+ 1) `fuse` foldC (+) 0
        actual `shouldBe` expected

fixBenches m =
    map (\(x, y) -> map (\(k, v) -> (k, Map.singleton x v)) $ Map.toList y) $ Map.toList m

thousandRef = unsafePerformIO $ newIORef 1000

toBench name run run2 = bgroup name
    [ bench "mapM_ yield, pure sum" $ flip whnf [1..1000] $ \is ->
        runIdentity $ run $ mapM_ yield is `fuse` foldC (+) 0
    , bench "mapM_ yield, monadic sum" $ whnfIO $ do
        x <- readIORef thousandRef
        run2 $ mapM_ yield [1..x] `fuse` foldC (+) 0
    , bench "mapM_ yield, map, pure sum" $ flip whnf [1..1000] $ \is ->
        runIdentity $ run $ mapM_ yield is `fuse` mapC (+ 1) `fuse` foldC (+) 0
    , bench "mapM_ yield, map, monadic sum" $ whnfIO $ do
        x <- readIORef thousandRef
        run2 $ mapM_ yield [1..x] `fuse` mapC (+ 1) `fuse` foldC (+) 0
    , bench "enumFromTo, pure sum" $ flip whnf 1000 $ \i ->
        runIdentity $ run $ enumFromToC 1 i `fuse` foldC (+) 0
    , bench "mapM_ yield, monadic sum" $ whnfIO $ do
        x <- readIORef thousandRef
        run2 $ enumFromToC 1 x `fuse` foldC (+) 0
    , bench "mapM_ yield, map, pure sum" $ flip whnf 1000 $ \i ->
        runIdentity $ run $ enumFromToC 1 i `fuse` mapC (+ 1) `fuse` foldC (+) 0
    , bench "mapM_ yield, map, monadic sum" $ whnfIO $ do
        x <- readIORef thousandRef
        run2 $ enumFromToC 1 x `fuse` mapC (+ 1) `fuse` foldC (+) 0
    ]

main :: IO ()
main = do
    withArgs [] $ hspec $ do
        toSpec "Standard" Conduit.Standard.run Conduit.Standard.run
        toSpec "Free" Conduit.Free.run Conduit.Free.run
        toSpec "Codensity" Conduit.Codensity.run Conduit.Codensity.run

    defaultMain $
        [ toBench "Standard" Conduit.Standard.run Conduit.Standard.run
        , toBench "Free" Conduit.Free.run Conduit.Free.run
        , toBench "Codensity" Conduit.Codensity.run Conduit.Codensity.run
        ]
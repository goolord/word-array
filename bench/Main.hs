{-# LANGUAGE
    BangPatterns
  , GADTs
  , DeriveGeneric
  , StandaloneDeriving
  , MagicHash
  , DataKinds
  , GeneralizedNewtypeDeriving
  , TypeApplications
  , ScopedTypeVariables
#-}

{-# OPTIONS_GHC
    -fno-warn-orphans
#-}

import Control.Monad.Primitive
import Data.Foldable (for_)
import Data.Functor.Identity (Identity(Identity))
import Data.Primitive
import Data.Word
import Data.Word64Array.Word8
import Test.Tasty.Bench (bench, bgroup, defaultMain, env, nf, nfIO)

{-# NOINLINE someArray #-}
someArray :: WordArray
someArray = toWordArray (maxBound `div` 2)

{-# NOINLINE mkPrimArray #-}
mkPrimArray :: IO (MutablePrimArray (PrimState IO) Word8)
mkPrimArray = do
  arr <- newPrimArray 8
  setPrimArray arr 0 8 (0 :: Word8)
  pure arr

{-# INLINE overIndexPA #-}
overIndexPA :: (Prim a, PrimMonad m) => Int -> (a -> a) -> MutablePrimArray (PrimState m) a -> m ()
overIndexPA i f arr = do
  v <- readPrimArray arr i
  writePrimArray arr i (f v)

{-# INLINE iforPrimArray #-}
iforPrimArray :: (Prim a, PrimMonad m) => MutablePrimArray (PrimState m) a -> (Int -> a -> m ()) -> m ()
iforPrimArray arr f = for_ [0,1,2,3,4,5,6,7] $ \i -> do
  v <- readPrimArray arr i
  f i v

main :: IO ()
main = do

  defaultMain
    [ bgroup "word-array"
        [ bench "overIndex" $ nf (overIndex 0 (+1)) someArray
        , bench "ifor" $ nf (flip iforWordArray (\i w -> i `seq` w `seq` Identity ())) (toWordArray maxBound)
        ]
      , bgroup "prim-array"
        [ env mkPrimArray $ \arr -> bench "overIndex" $ nfIO (overIndexPA 0 (+1) arr)
        , env mkPrimArray $ \arr -> bench "ifor" $ nfIO (flip iforPrimArray (\i w -> i `seq` w `seq` pure ()) arr)
        ]
    ]

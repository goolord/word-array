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
import Data.Functor.Identity (Identity(Identity))
import Data.Primitive
import Data.Word
import Data.Word64Array.Word8
import Test.Tasty.Bench (bench, bgroup, defaultMain, env, nf, nfIO)
import Control.DeepSeq (deepseq, NFData (rnf))
import Control.Monad.Trans.State.Strict

{-# NOINLINE someArray #-}
someArray :: WordArray
someArray = toWordArray (maxBound `div` 2)

{-# NOINLINE mkPrimArray #-}
mkPrimArray :: IO (MutablePrimArray (PrimState IO) Word8)
mkPrimArray = do
  arr <- newPrimArray 7
  setPrimArray arr 0 7 (0 :: Word8)
  pure arr

{-# INLINE overIndexPA #-}
overIndexPA :: (Prim a, PrimMonad m) => Int -> (a -> a) -> MutablePrimArray (PrimState m) a -> m ()
overIndexPA i f arr = do
  v <- readPrimArray arr i
  writePrimArray arr i (f v)

{-# NOINLINE iforPrimArray #-}
iforPrimArray :: (Prim a, PrimMonad m) => MutablePrimArray (PrimState m) a -> (Int -> a -> m ()) -> m ()
iforPrimArray arr f = do
  readPrimArray arr 0 >>= f 0
  readPrimArray arr 1 >>= f 1
  readPrimArray arr 2 >>= f 2
  readPrimArray arr 3 >>= f 3
  readPrimArray arr 4 >>= f 4
  readPrimArray arr 5 >>= f 5
  readPrimArray arr 6 >>= f 6
  readPrimArray arr 7 >>= f 7

{-# NOINLINE iforWordArray' #-}
iforWordArray' :: Applicative f => WordArray
  -> (Int -> Word8 -> f ())
  -> f ()
iforWordArray' = iforWordArray

sumState :: Monad m => Int -> Word8 -> StateT Int m ()
sumState i w = 
  modify $ \s -> i + s + fromIntegral w

main :: IO ()
main = do

  defaultMain
    [ bgroup "word-array"
        [ bench "overIndex" $ nf (overIndex 0 (+1)) someArray
        , bench "ifor" $ nf (flip iforWordArray (\i w -> i `deepseq` w `deepseq` Identity ())) (toWordArray maxBound)
        , bench "ifor sum" $ nf (flip runState 0 . flip iforWordArray sumState) (toWordArray maxBound)
        ]
      , bgroup "prim-array"
        [ env mkPrimArray $ \arr -> bench "overIndex" $ nfIO (overIndexPA 0 (+1) arr)
        , env mkPrimArray $ \arr -> bench "ifor" $ nfIO (iforPrimArray arr (\i w -> i `deepseq` w `deepseq` pure ()))
        , env mkPrimArray $ \arr -> bench "ifor sum" $ nfIO $ do
            (_, !i) <- flip runStateT 0 $ iforPrimArray arr sumState
            pure i
        ]
    ]

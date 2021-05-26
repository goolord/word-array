{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE
    MagicHash
  , TypeOperators
  , DataKinds
  , KindSignatures
  , TypeFamilies
  , StandaloneDeriving
  , GeneralizedNewtypeDeriving
  , TypeApplications
  , ScopedTypeVariables
  , InstanceSigs
#-}

{-# LANGUAGE RankNTypes #-}
module Data.Word64Array.Word8 where

import Data.Primitive.ByteArray
import Data.MonoTraversable
import Data.Word
import GHC.ST (runST)
import Data.Primitive
import Data.Maybe (fromMaybe)
import Data.Bits
import GHC.Exts (toList)

newtype WordArray        = WordArray ByteArray
newtype WordArrayBitwise = WordArrayBitwise Word64

type instance Element WordArray        = Word8
type instance Element WordArrayBitwise = Word8

-- | endienness is platform specific
{-# INLINE toWordArray #-}
toWordArray :: Word64 -> WordArray
toWordArray w = WordArray $ runST $ do
  arr <- newByteArray (sizeOf w)
  writeByteArray arr 0 w
  unsafeFreezeByteArray arr

{-# INLINE fromWordArray #-}
fromWordArray :: WordArray -> Word64
fromWordArray (WordArray ba) = indexByteArray ba 0

{-# INLINE overIndex #-}
overIndex :: Int -> (Word8 -> Word8) -> WordArray -> WordArray
overIndex !i f (WordArray !ba) = WordArray $ runST $ do
  arr <- newByteArray (sizeOf (undefined :: Word64))
  copyByteArray arr 0 ba 0 (sizeOf (undefined :: Word64))
  x <- readByteArray arr i
  writeByteArray arr i (f x)
  unsafeFreezeByteArray arr

{-# INLINE overIndexBitwise #-}
overIndexBitwise :: Int -> (Word8 -> Word8) -> WordArrayBitwise -> WordArrayBitwise
overIndexBitwise i f (WordArrayBitwise w) =
  let w8 = fromIntegral $ unsafeShiftR w (8 - (i * 8))
      w8' = f w8
      w64 :: Word64
      w64 = shiftL (fromIntegral w8') (8 - (i * 8))
  in WordArrayBitwise (w + w64)

{-# INLINE iforWordArray #-}
iforWordArray :: Applicative f => WordArray -> (Int -> Word8 -> f ()) -> f ()
iforWordArray (WordArray ba) f =
  let [!w0,!w1,!w2,!w3,!w4,!w5,!w6,!w7] = toList ba
  in   f 0 (fromIntegral w0) 
    *> f 1 (fromIntegral w1) 
    *> f 2 (fromIntegral w2) 
    *> f 3 (fromIntegral w3) 
    *> f 4 (fromIntegral w4) 
    *> f 5 (fromIntegral w5) 
    *> f 6 (fromIntegral w6) 
    *> f 7 (fromIntegral w7)

{-# INLINE iforWordArrayBitwise #-}
iforWordArrayBitwise :: Applicative f => WordArrayBitwise -> (Int -> Word8 -> f ()) -> f ()
iforWordArrayBitwise (WordArrayBitwise !w) f =
  let
    !w0 = w
    !w1 = unsafeShiftR w0 8
    !w2 = unsafeShiftR w1 8
    !w3 = unsafeShiftR w2 8
    !w4 = unsafeShiftR w3 8
    !w5 = unsafeShiftR w4 8
    !w6 = unsafeShiftR w5 8
    !w7 = unsafeShiftR w6 8
  in   f 0 (fromIntegral w0) 
    *> f 1 (fromIntegral w1) 
    *> f 2 (fromIntegral w2) 
    *> f 3 (fromIntegral w3) 
    *> f 4 (fromIntegral w4) 
    *> f 5 (fromIntegral w5) 
    *> f 6 (fromIntegral w6) 
    *> f 7 (fromIntegral w7)

instance MonoFoldable WordArray where
  ofoldr f b (WordArray ba) = foldrByteArray f b ba
  ofoldl' f z0 xs = ofoldr f' id xs z0
    where f' x k z = k $! f z x
  ofoldMap f = ofoldr (mappend . f) mempty
  onull _ = False
  oelem e = ofoldr (\a b -> a == e || b) False
  ofoldr1Ex f xs = fromMaybe 
      (errorWithoutStackTrace "error in word-array ofoldr1Ex: empty array")
      (ofoldr mf Nothing xs)
    where
      mf x m = Just (case m of
                       Nothing -> x
                       Just y  -> f x y)
  ofoldl1Ex' f xs = fromMaybe 
      (errorWithoutStackTrace "error in word-array ofoldr1Ex: empty array")
      (ofoldl' mf Nothing xs)
    where
      mf m y = Just (case m of
                       Nothing -> y
                       Just x  -> f x y)

{-# LANGUAGE
    MagicHash
  , TypeOperators
  , DataKinds
  , BangPatterns
  , KindSignatures
  , TypeFamilies
  , StandaloneDeriving
  , GeneralizedNewtypeDeriving
  , TypeApplications
  , ScopedTypeVariables
  , InstanceSigs
  , BinaryLiterals
  , RankNTypes
  , UnboxedTuples
#-}

module Data.Word64Array.Word8
  ( WordArray(..)
  , Index(..)
  , toWordArray
  , readArray
  , writeArray
  , overIndex
  , iforWordArray
  , toList
  , toTuple
  , fromTuple
  , displayWordArray
  ) where

import Control.DeepSeq
import Data.MonoTraversable
import Data.Word
import Data.Maybe (fromMaybe)
import Data.Bits
import Numeric (showHex)
import Text.Show (showListWith)

{- Note [Representation of WordArray]
WordArray has its constituent Word8s packed in order from *left-to-right*, i.e.
the first Word8 occupies the most-significant bits.

Hence the offset to find the start of the ith Word8 is (-8*i) + 56.
-}

newtype WordArray = WordArray { fromWordArray :: Word64 }
  deriving (Show, Eq, Ord, NFData)

type instance Element WordArray = Word8

newtype Index = Index { getIndex :: Int }
  deriving (Show, Num, Eq, Ord)

instance Bounded Index where
  maxBound = 7
  minBound = 0

{-# INLINE toWordArray #-}
toWordArray :: Word64 -> WordArray
toWordArray = WordArray

displayWordArray :: WordArray -> String
displayWordArray wa = displayWordArrayS wa ""
  where
  displayHex x s = "0x" <> showHex x s
  displayWordArrayS = showListWith displayHex . toList

{-# INLINE toTuple #-}
toTuple :: WordArray -> (# Element WordArray, Element WordArray, Element WordArray, Element WordArray, Element WordArray, Element WordArray, Element WordArray, Element WordArray #)
toTuple (WordArray !w) = 
  let
    !w7 = w
    !w6 = unsafeShiftR w7 8
    !w5 = unsafeShiftR w6 8
    !w4 = unsafeShiftR w5 8
    !w3 = unsafeShiftR w4 8
    !w2 = unsafeShiftR w3 8
    !w1 = unsafeShiftR w2 8
    !w0 = unsafeShiftR w1 8
  in 
  (# fromIntegral w0
  ,  fromIntegral w1
  ,  fromIntegral w2
  ,  fromIntegral w3
  ,  fromIntegral w4
  ,  fromIntegral w5
  ,  fromIntegral w6
  ,  fromIntegral w7
  #)

{-# INLINE fromTuple #-}
fromTuple :: (# Element WordArray, Element WordArray, Element WordArray, Element WordArray, Element WordArray, Element WordArray, Element WordArray, Element WordArray #) -> WordArray
fromTuple (# !w0, !w1, !w2, !w3, !w4, !w5, !w6, !w7 #) =
    WordArray
      (                (fromIntegral w7)
      .|. unsafeShiftL (fromIntegral w6) 8
      .|. unsafeShiftL (fromIntegral w5) 16
      .|. unsafeShiftL (fromIntegral w4) 24
      .|. unsafeShiftL (fromIntegral w3) 32
      .|. unsafeShiftL (fromIntegral w2) 40
      .|. unsafeShiftL (fromIntegral w1) 48
      .|. unsafeShiftL (fromIntegral w0) 56
      )

{-# INLINE toList #-}
toList :: WordArray -> [Element WordArray]
toList w =
  let (# !w0, !w1, !w2, !w3, !w4, !w5, !w6, !w7 #) = toTuple w
  in [w0, w1, w2, w3, w4, w5, w6, w7]

{-# INLINE readArray #-}
readArray :: WordArray -> Index -> Element WordArray
readArray (WordArray !w) !i =
  -- See Note [Representation of WordArray]
  fromIntegral $ unsafeShiftR w (offset i)

{-# INLINE offset #-}
offset :: Index -> Int
offset !i = (-8 * getIndex i) + 56

{-# INLINE writeArray #-}
writeArray :: WordArray -> Index -> Element WordArray -> WordArray
writeArray (WordArray !w) !i !w8 =
  -- See Note [Representation of WordArray]
  let w64 :: Word64
      w64 = unsafeShiftL (fromIntegral w8) (offset i)
  in WordArray ((w .&. mask i) + w64)

{-# INLINE overIndex #-}
-- | Modify the word at a given index.
overIndex :: Index -> (Element WordArray -> Element WordArray) -> WordArray -> WordArray
overIndex !i f !w = writeArray w i $ f $ readArray w i

{-# INLINE mask #-}
mask :: Index -> Word64
mask 0 = 0x00ffffffffffffff
mask 1 = 0xff00ffffffffffff
mask 2 = 0xffff00ffffffffff
mask 3 = 0xffffff00ffffffff
mask 4 = 0xffffffff00ffffff
mask 5 = 0xffffffffff00ffff
mask 6 = 0xffffffffffff00ff
mask 7 = 0xffffffffffffff00
mask _ = error "mask"

{-# INLINE iforWordArray #-}
iforWordArray :: Applicative f => WordArray -> (Int -> Element WordArray -> f ()) -> f ()
iforWordArray !w f =
  let (# !w0, !w1, !w2, !w3, !w4, !w5, !w6, !w7 #) = toTuple w
  in   f 0 w0 *> f 1 w1 *> f 2 w2 *> f 3 w3 
    *> f 4 w4 *> f 5 w5 *> f 6 w6 *> f 7 w7

instance MonoFunctor WordArray where
  omap f w =
    let (# !w0, !w1, !w2, !w3, !w4, !w5, !w6, !w7 #) = toTuple w
    in fromTuple (# f w0, f w1, f w2, f w3, f w4, f w5, f w6, f w7 #)

instance MonoFoldable WordArray where
  {-# INLINE ofoldr #-}
  ofoldr f !b !w =
    let (# !w0, !w1, !w2, !w3, !w4, !w5, !w6, !w7 #) = toTuple w
    in  f w0 $ f w1 $ f w2 $ f w3 $ f w4 $ f w5 $ f w6 $ f w7 b
  {-# INLINE ofoldl' #-}
  ofoldl' f z0 xs = ofoldr f' id xs z0
    where f' x k z = k $! f z x
  {-# INLINE ofoldMap #-}
  ofoldMap f = ofoldr (mappend . f) mempty
  {-# INLINE onull #-}
  onull _ = False
  {-# INLINE oelem #-}
  oelem e = ofoldr (\a b -> a == e || b) False
  {-# INLINE ofoldr1Ex #-}
  ofoldr1Ex f xs = fromMaybe 
      (errorWithoutStackTrace "error in word-array ofoldr1Ex: empty array")
      (ofoldr mf Nothing xs)
    where
    mf x m = Just $ case m of
      Nothing -> x
      Just y  -> f x y
  {-# INLINE ofoldl1Ex' #-}
  ofoldl1Ex' f xs = fromMaybe 
      (errorWithoutStackTrace "error in word-array ofoldr1Ex: empty array")
      (ofoldl' mf Nothing xs)
    where
    mf m y = Just $ case m of
      Nothing -> y
      Just x  -> f x y

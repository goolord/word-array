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

import Test.Tasty.Bench (defaultMain, bench, nf)

import Data.Word
import Control.DeepSeq
import Unsafe.Coerce
import Control.Exception
import qualified Weigh
import Data.Monoid
import Data.Word64Array.Word8
import Data.Functor.Const

deriving instance NFData (WordArray)
deriving instance NFData (WordArrayBitwise)

main :: IO ()
main = do
  defaultMain
    [ bench "toWordArray" $ nf toWordArray maxBound
    , bench "fromWordArray" $ nf fromWordArray (toWordArray maxBound)
    , bench "overIndexBitwise" $ nf (overIndexBitwise 0 (+1)) (WordArrayBitwise maxBound)
    , bench "overIndex" $ nf (overIndex 0 (+1)) (toWordArray maxBound)
    , bench "iforWordArrayBitwise" $ nf (flip iforWordArrayBitwise (\i w -> Const $ Sum i)) (WordArrayBitwise maxBound)
    , bench "iforWordArray" $ nf (flip iforWordArray (\i w -> Const $ Sum i)) (toWordArray maxBound)
    ]
  putStr "Memory usage :"
  Weigh.mainWith $ do
    -- Weigh.action "Word8" w8
    -- Weigh.action "WordN 8" wn8
    -- Weigh.action "OddWord 8 Word8" oddword8
    pure ()


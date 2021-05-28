{-# LANGUAGE 
    MagicHash 
  , TemplateHaskell
  , TypeApplications
  , DataKinds
  , StandaloneDeriving
  , GeneralizedNewtypeDeriving
#-}

{-# OPTIONS_GHC
    -fno-warn-orphans
#-}

module Main where

-- import Test.Tasty.HUnit
import Data.MonoTraversable
import Data.Proxy
import Data.Vector
import Data.Word
import Data.Word64Array.Word8 as WordArray
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Data.Word64Array.Word8 as W64A
deriving instance Arbitrary W64A.WordArray

toVector :: WordArray -> Vector Word8
toVector = fromList . WordArray.toList

instance Arbitrary Index where
  arbitrary = Index <$> choose (0, 7)
  shrink (Index i) = if i == 0 then [] else [Index (i-1)]

main :: IO ()
main = do
  lawsCheckMany typeclassLaws
  defaultMain tests

tests :: TestTree
tests = 
  testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testGroup "word-array" [
        testProperty "write" $ \(arr, ix@(Index i), w8) ->
            toVector (writeArray arr ix w8) == toVector arr // [(i, w8)]
        , testProperty "read" $ \(arr, ix@(Index i)) ->
            readArray arr ix == toVector arr ! i
        , testProperty "fold" $ \arr ->
            ofoldr (+) 0 arr == Prelude.foldr (+) 0 (toVector arr)
    ]
  ]

w64w8Laws :: [Laws]
w64w8Laws =
  let p = Proxy :: Proxy W64A.WordArray
  in [eqLaws p, ordLaws p]

typeclassLaws :: ([(String, [Laws])])
typeclassLaws = 
  [ ("Word64Array Word8", w64w8Laws)
  ]

unwrap :: Either a b -> b
unwrap = either (error "unwrap") id

testLaws :: Laws -> TestTree
testLaws (Laws tc lp) = testProperties tc lp


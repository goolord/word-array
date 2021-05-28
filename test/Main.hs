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

import Test.Tasty
-- import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.Classes
import Data.Proxy
import qualified Data.Word64Array.Word8 as W64A

deriving instance Arbitrary W64A.WordArray

main :: IO ()
main = do
  lawsCheckMany typeclassLaws
  defaultMain tests

tests :: TestTree
tests = 
  testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [
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


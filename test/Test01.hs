module Test01 where

import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Day01 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [part1Tests]

example :: Vector (Vector Word)
example = Vec.fromList $ fmap Vec.fromList
    [ [1000, 2000, 3000]
    , [4000]
    , [5000, 6000]
    , [7000, 8000, 9000]
    , [10000] ]

part1Tests :: TestTree
part1Tests = Tasty.testGroup "part 1 tests"
    [ HUnit.testCase "example" $
        Day01.part1 example @?= 24_000 ]

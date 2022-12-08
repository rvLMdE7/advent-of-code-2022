module Test08 where

import Data.Vector qualified as Vec
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Day08 qualified
import Day08 (Matrix)


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [part1Tests]

example :: Matrix Int
example = Vec.fromList $ fmap Vec.fromList
    [ [3, 0, 3, 7, 3]
    , [2, 5, 5, 1, 2]
    , [6, 5, 3, 3, 2]
    , [3, 3, 5, 4, 9]
    , [3, 5, 3, 9, 0] ]

part1Tests :: TestTree
part1Tests = Tasty.testGroup "part 1 tests"
    [ HUnit.testCase "example # visible" $
        Day08.part1 example @?= 21 ]

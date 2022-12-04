{-# LANGUAGE OverloadedStrings #-}

module Test04 where

import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Text.Megaparsec qualified as Parse

import Day04 qualified
import Day04 (Range(..))


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [part1Tests, part2Tests]

example :: Vector (Range, Range)
example = Vec.fromList
    [ (MkRange 2 4, MkRange 6 8)
    , (MkRange 2 3, MkRange 4 5)
    , (MkRange 5 7, MkRange 7 9)
    , (MkRange 2 8, MkRange 3 7)
    , (MkRange 6 6, MkRange 4 6)
    , (MkRange 2 6, MkRange 4 8) ]

part1Tests :: TestTree
part1Tests = Tasty.testGroup "part 1 tests"
    [ HUnit.testCase "can parse example" $
        Parse.runParser Day04.parseRangePairs "test" text @?= Right example
    , HUnit.testCase "nested ranges" $
        Vec.map (uncurry Day04.nested) example
            @?= Vec.fromList [False, False, False, True, True, False]
    , HUnit.testCase "count nested ranges" $
        Day04.part1 example @?= 2 ]
  where
    text =
        "2-4,6-8\n\
        \2-3,4-5\n\
        \5-7,7-9\n\
        \2-8,3-7\n\
        \6-6,4-6\n\
        \2-6,4-8\n"

part2Tests :: TestTree
part2Tests = Tasty.testGroup "part 2 tests"
    [ HUnit.testCase "overlaps" $
        Vec.map (uncurry Day04.overlap) example
            @?= Vec.fromList [False, False, True, True, True, True] ]

{-# LANGUAGE OverloadedStrings #-}

module Test05 where

import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Text.Megaparsec qualified as Parse

import Day05 qualified
import Day05 (Chars)


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [part1Tests, part2Tests]

exampleStacks :: Vector Chars
exampleStacks = Vec.fromList ["NZ", "DCM", "P"]

exampleMoves :: Vector (Int, Int, Int)
exampleMoves = Vec.fromList [(1,1,0), (3,0,2), (2,1,0), (1,0,1)]

part1Tests :: TestTree
part1Tests = Tasty.testGroup "part 1 tests"
    [ HUnit.testCase "parse example" $
        Parse.runParser Day05.parseInput "test" text
            @?= Right (exampleStacks, exampleMoves)
    , HUnit.testCase "perform moves" $
        Day05.apply Day05.moveN exampleMoves exampleStacks
            @?= Right (Vec.fromList ["C", "M", "ZNDP"])
    , HUnit.testCase "check top of stacks" $
        Day05.part1 exampleMoves exampleStacks
            @?= Right (Vec.fromList "CMZ") ]
  where
    text =
        "    [D]    \n\
        \[N] [C]    \n\
        \[Z] [M] [P]\n\
        \ 1   2   3 \n\
        \\n\
        \move 1 from 2 to 1\n\
        \move 3 from 1 to 3\n\
        \move 2 from 2 to 1\n\
        \move 1 from 1 to 2\n"

part2Tests :: TestTree
part2Tests = Tasty.testGroup "part 2 tests"
    [ HUnit.testCase "check top of stacks" $
        Day05.part2 exampleMoves exampleStacks
            @?= Right (Vec.fromList "MCD") ]

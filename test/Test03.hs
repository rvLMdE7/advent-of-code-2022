{-# LANGUAGE OverloadedStrings #-}

module Test03 where

import Control.Arrow ((>>>))
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Text.Megaparsec qualified as Parse

import Day03 qualified
import Day03 (Rucksack(..))


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [part1Tests, part2Tests]

example :: Vector Rucksack
example = Vec.fromList
    [ MkRucksack "vJrwpWtwJgWr" "hcsFMMfFFhFp" 'p'
    , MkRucksack "jqHRNqRjqzjGDLGL" "rsFMfFZSrLrFZsSL" 'L'
    , MkRucksack "PmmdzqPrV" "vPwwTWBwg" 'P'
    , MkRucksack "wMqvLMZHhHMvwLH" "jbvcjnnSBnvTQFn" 'v'
    , MkRucksack "ttgJtRGJ" "QctTZtZT" 't'
    , MkRucksack "CrZsJsPPZsGz" "wwsLwLmpwMDw" 's' ]

part1Tests :: TestTree
part1Tests = Tasty.testGroup "part 1 tests"
    [ HUnit.testCase "can parse example" $
        Parse.runParser Day03.parseRucksacks "test" text @?= Right example
    , HUnit.testCase "example shared items" $
        fmap shared example @?= Vec.fromList "pLPvts"
    , HUnit.testCase "lowercase priority" $
        fmap Day03.priority ['a'..'z'] @?= [1..26]
    , HUnit.testCase "uppercase priority" $
        fmap Day03.priority ['A'..'Z'] @?= [27..52]
    , HUnit.testCase "example priorities" $
        fmap (shared >>> Day03.priority) example
            @?= Vec.fromList [16, 38, 42, 22, 20, 19]
    , HUnit.testCase "example total priority" $
        Day03.part1 example @?= 157 ]
  where
    text =
        "vJrwpWtwJgWrhcsFMMfFFhFp\n\
        \jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n\
        \PmmdzqPrVvPwwTWBwg\n\
        \wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n\
        \ttgJtRGJQctTZtZT\n\
        \CrZsJsPPZsGzwwsLwLmpwMDw\n"

part2Tests :: TestTree
part2Tests = Tasty.testGroup "part 2 tests"
    [ HUnit.testCase "badge for group 1" $
        Day03.badge (bundled Vec.! 0) @?= Just 'r'
    , HUnit.testCase "badge for group 2" $
        Day03.badge (bundled Vec.! 1) @?= Just 'Z'
    , HUnit.testCase "example total badge priority" $
        Day03.part2 example @?= Just 70 ]
  where
    bundled = Day03.bundle3 example

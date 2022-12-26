{-# LANGUAGE OverloadedStrings #-}

module Test12 where

import Data.Functor ((<&>))
import Data.Matrix (Matrix)
import Data.Matrix qualified as Matrix
import Linear (V2(V2))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Text.Megaparsec qualified as Parse

import Day12 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [part1Tests]

exampleMatrix :: Matrix Char
exampleMatrix = Matrix.fromLists
    [ "aabqponm"
    , "abcryxxl"
    , "accszzxk"
    , "acctuvwj"
    , "abdefghi" ]

part1Tests :: TestTree
part1Tests = Tasty.testGroup "part 1 tests"
    [ HUnit.testCase "parse hill" $
        Parse.runParser (Day12.parseHill <* Parse.eof) "test" text
            @?= Right (V2 1 1, V2 6 3, exampleMatrix)
    , HUnit.testCase "shortest path start to end" $
        pathLength (V2 1 1) (V2 6 3) @?= Just 31 ]
  where
    -- subtract one, as the shortestPath function returns the full path
    -- including start & end points
    pathLength u v =
        Day12.shortestPath u v exampleMatrix
            <&> \path -> length path - 1
    text =
        "Sabqponm\n\
        \abcryxxl\n\
        \accszExk\n\
        \acctuvwj\n\
        \abdefghi\n"

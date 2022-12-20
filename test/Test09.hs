module Test09 where

import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Day09 qualified
import Day09 (Move(..), Direction(..))


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [part1Tests]

example :: Vector Move
example = Vec.fromList
    [ MkMove {direction = RightDir, distance = 4}
    , MkMove {direction = UpDir,    distance = 4}
    , MkMove {direction = LeftDir,  distance = 3}
    , MkMove {direction = DownDir,  distance = 1}
    , MkMove {direction = RightDir, distance = 4}
    , MkMove {direction = DownDir,  distance = 1}
    , MkMove {direction = LeftDir,  distance = 5}
    , MkMove {direction = RightDir, distance = 2} ]

part1Tests :: TestTree
part1Tests = Tasty.testGroup "part 1 tests"
    [ HUnit.testCase "example" $
        Day09.part1 example @?= 13 ]

part2Tests :: TestTree
part2Tests = Tasty.testGroup "part 2 tests"
    [ HUnit.testCase "part 1 example" $
        Day09.part2 example @?= 1
    , HUnit.testCase "larger example" $
        Day09.part2 larger @?= 36 ]
  where
    larger = Vec.fromList
        [ MkMove {direction = RightDir, distance = 5}
        , MkMove {direction = UpDir,    distance = 8}
        , MkMove {direction = LeftDir,  distance = 8}
        , MkMove {direction = DownDir,  distance = 3}
        , MkMove {direction = RightDir, distance = 17}
        , MkMove {direction = DownDir,  distance = 10}
        , MkMove {direction = LeftDir,  distance = 25}
        , MkMove {direction = UpDir,    distance = 20} ]

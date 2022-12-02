module Test02 where

import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Day02 qualified
import Day02 (Round(..), Opponent(..), Player(..))


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [part1Tests]

guide :: Vector Round
guide = Vec.fromList
    [ MkRound {opponent = A, player = Y}
    , MkRound {opponent = B, player = X}
    , MkRound {opponent = C, player = Z} ]

part1Tests :: TestTree
part1Tests = Tasty.testGroup "part 1 tests"
    [ HUnit.testCase "each round's score" $
        Vec.map Day02.scoreRound guide @?= Vec.fromList [8, 1, 6]
    , HUnit.testCase "total score" $
        Day02.part1 guide @?= 15 ]

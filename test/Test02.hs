module Test02 where

import Control.Arrow ((>>>))
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Day02 qualified
import Day02 (Encrypted, Generic(..), Opponent(..), Player(..))


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [part1Tests, part2Tests]

guide :: Vector Encrypted
guide = Vec.fromList
    [ MkGeneric {opponent = A, player = Y}
    , MkGeneric {opponent = B, player = X}
    , MkGeneric {opponent = C, player = Z} ]

part1Tests :: TestTree
part1Tests = Tasty.testGroup "part 1 tests"
    [ HUnit.testCase "each round's score" $
        Vec.map (Day02.decrypt1 >>> Day02.scoreRound) guide
            @?= Vec.fromList [8, 1, 6]
    , HUnit.testCase "total score" $
        Day02.part1 guide @?= 15 ]

part2Tests :: TestTree
part2Tests = Tasty.testGroup "part 2 tests"
    [ HUnit.testCase "each round's score" $
        Vec.map (Day02.decrypt2 >>> Day02.scoreRound) guide
            @?= Vec.fromList [4, 1, 7]
    , HUnit.testCase "total score" $
        Day02.part2 guide @?= 12 ]

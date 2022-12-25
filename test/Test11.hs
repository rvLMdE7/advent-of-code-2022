{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test11 where

import Control.Arrow ((>>>), (&&&))
import Control.Monad (replicateM_)
import Control.Monad.State (execState)
import Data.Foldable qualified as Fold
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Sequence qualified as Seq
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Text.Megaparsec qualified as Parse

import Day11 qualified
import Day11 (Monkey)


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [part1Tests]

exampleMonkeys :: IntMap Monkey
exampleMonkeys = IntMap.fromList
    [ keyVal $ mkMonkey 0 [79, 98] (* 19) 23 (2, 3)
    , keyVal $ mkMonkey 1 [54, 65, 75, 74] (+ 6) 19 (2, 0)
    , keyVal $ mkMonkey 2 [79, 60, 97] (^ id @Int 2) 13 (1, 3)
    , keyVal $ mkMonkey 3 [74] (+ 3) 17 (0, 1) ]
  where
    mkMonkey num items op quotient (t, f) = Day11.MkMonkey
        { number = num
        , items = Seq.fromList items
        , operation = op
        , throwTo = \n -> if n `mod` quotient == 0 then t else f
        , inspected = 0 }
    keyVal = Day11.number &&& id

part1Tests :: TestTree
part1Tests = Tasty.testGroup "part 1 tests"
    [ HUnit.testCase "parse monkeys" $
        let parsed = Parse.runParser (Day11.parseMonkeys <* Parse.eof) "" text
        in  fmap reprMonkeys parsed @?= Right (reprMonkeys exampleMonkeys)
    , HUnit.testCase "round 1" $
        reprMonkeys (execRound 1) @?= zip [0..]
            [ [20, 23, 27, 26]
            , [2080, 25, 167, 207, 401, 1046]
            , []
            , [] ]
    , HUnit.testCase "round 5" $
        reprMonkeys (execRound 5) @?= zip [0..]
            [ [15, 17, 16, 88, 1037]
            , [20, 110, 205, 524, 72]
            , []
            , [] ]
    , HUnit.testCase "round 10" $
        reprMonkeys (execRound 10) @?= zip [0..]
            [ [91, 16, 20, 98]
            , [481, 245, 22, 26, 1092, 30]
            , []
            , [] ]
    , HUnit.testCase "round 15" $
        reprMonkeys (execRound 15) @?= zip [0..]
            [ [83, 44, 8, 184, 9, 20, 26, 102]
            , [110, 36]
            , []
            , [] ]
    , HUnit.testCase "round 20" $
        reprMonkeys (execRound 20) @?= zip [0..]
            [ [10, 12, 14, 26, 34]
            , [245, 93, 53, 199, 115]
            , []
            , [] ]
    , HUnit.testCase "inspected" $
        fmap Day11.inspected (IntMap.elems $ execRound 20)
            @?= [101, 95, 7, 105] ]
  where
    execRound n = execState (replicateM_ n Day11.round) exampleMonkeys
    reprMonkeys = IntMap.elems >>> fmap reprMonkey
    reprMonkey Day11.MkMonkey{..} = (number, Fold.toList items)
    text =
        "Monkey 0:\n\
        \  Starting items: 79, 98\n\
        \  Operation: new = old * 19\n\
        \  Test: divisible by 23\n\
        \    If true: throw to monkey 2\n\
        \    If false: throw to monkey 3\n\
        \\n\
        \Monkey 1:\n\
        \  Starting items: 54, 65, 75, 74\n\
        \  Operation: new = old + 6\n\
        \  Test: divisible by 19\n\
        \    If true: throw to monkey 2\n\
        \    If false: throw to monkey 0\n\
        \\n\
        \Monkey 2:\n\
        \  Starting items: 79, 60, 97\n\
        \  Operation: new = old * old\n\
        \  Test: divisible by 13\n\
        \    If true: throw to monkey 1\n\
        \    If false: throw to monkey 3\n\
        \\n\
        \Monkey 3:\n\
        \  Starting items: 74\n\
        \  Operation: new = old + 3\n\
        \  Test: divisible by 17\n\
        \    If true: throw to monkey 0\n\
        \    If false: throw to monkey 1\n"

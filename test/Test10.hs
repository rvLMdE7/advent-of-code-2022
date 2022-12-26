{-# LANGUAGE OverloadedStrings #-}

module Test10 where

import Control.Monad.State (evalState)
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Text.Megaparsec qualified as Parse

import Day10 qualified
import Day10 (Instr(..))


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [part1Tests, part2Tests]

exampleInstrs :: [Instr]
exampleInstrs =
    [ Addx 15, Addx (-11), Addx 6, Addx (-3), Addx 5, Addx (-1), Addx (-8)
    , Addx 13, Addx 4, Noop, Addx (-1), Addx 5, Addx (-1), Addx 5, Addx (-1)
    , Addx 5, Addx (-1), Addx 5, Addx (-1), Addx (-35), Addx 1, Addx 24
    , Addx (-19), Addx 1, Addx 16, Addx (-11), Noop, Noop, Addx 21
    , Addx (-15), Noop, Noop, Addx (-3), Addx 9, Addx 1, Addx (-3), Addx 8
    , Addx 1, Addx 5, Noop, Noop, Noop, Noop, Noop, Addx (-36), Noop, Addx 1
    , Addx 7, Noop, Noop, Noop, Addx 2, Addx 6, Noop, Noop, Noop, Noop, Noop
    , Addx 1, Noop, Noop, Addx 7, Addx 1, Noop, Addx (-13), Addx 13, Addx 7
    , Noop, Addx 1, Addx (-33), Noop, Noop, Noop, Addx 2, Noop, Noop, Noop
    , Addx 8, Noop, Addx (-1), Addx 2, Addx 1, Noop, Addx 17, Addx (-9)
    , Addx 1, Addx 1, Addx (-3), Addx 11, Noop, Noop, Addx 1, Noop, Addx 1
    , Noop, Noop, Addx (-13), Addx (-19), Addx 1, Addx 3, Addx 26, Addx (-30)
    , Addx 12, Addx (-1), Addx 3, Addx 1, Noop, Noop, Noop, Addx (-9), Addx 18
    , Addx 1, Addx 2, Noop, Noop, Addx 9, Noop, Noop, Noop, Addx (-1), Addx 2
    , Addx (-37), Addx 1, Addx 3, Noop, Addx 15, Addx (-21), Addx 22
    , Addx (-6), Addx 1, Noop, Addx 2, Addx 1, Noop, Addx (-10), Noop, Noop
    , Addx 20, Addx 1, Addx 2, Addx 2, Addx (-6), Addx (-11), Noop, Noop
    , Noop ]

part1Tests :: TestTree
part1Tests = Tasty.testGroup "part 1 tests"
    [ HUnit.testCase "parse instructions" $
        Parse.runParser (Day10.parseInstrs <* Parse.eof) "foo" text
            @?= Right exampleInstrs
    , HUnit.testCase "signal strengths" $
        let prog = Day10.signalStrengthsAt [20, 60 .. 220]
        in  evalState prog (Day10.MkCpu 1 1 exampleInstrs)
                @?= [420, 1140, 1800, 2940, 2880, 3960] ]
  where
    text =
        "addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\n\
        \addx 13\naddx 4\nnoop\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\n\
        \addx 5\naddx -1\naddx 5\naddx -1\naddx -35\naddx 1\naddx 24\n\
        \addx -19\naddx 1\naddx 16\naddx -11\nnoop\nnoop\naddx 21\naddx -15\n\
        \noop\nnoop\naddx -3\naddx 9\naddx 1\naddx -3\naddx 8\naddx 1\n\
        \addx 5\nnoop\nnoop\nnoop\nnoop\nnoop\naddx -36\nnoop\naddx 1\n\
        \addx 7\nnoop\nnoop\nnoop\naddx 2\naddx 6\nnoop\nnoop\nnoop\nnoop\n\
        \noop\naddx 1\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13\n\
        \addx 7\nnoop\naddx 1\naddx -33\nnoop\nnoop\nnoop\naddx 2\nnoop\n\
        \noop\nnoop\naddx 8\nnoop\naddx -1\naddx 2\naddx 1\nnoop\naddx 17\n\
        \addx -9\naddx 1\naddx 1\naddx -3\naddx 11\nnoop\nnoop\naddx 1\n\
        \noop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1\naddx 3\n\
        \addx 26\naddx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\n\
        \noop\naddx -9\naddx 18\naddx 1\naddx 2\nnoop\nnoop\naddx 9\nnoop\n\
        \noop\nnoop\naddx -1\naddx 2\naddx -37\naddx 1\naddx 3\nnoop\n\
        \addx 15\naddx -21\naddx 22\naddx -6\naddx 1\nnoop\naddx 2\naddx 1\n\
        \noop\naddx -10\nnoop\nnoop\naddx 20\naddx 1\naddx 2\naddx 2\n\
        \addx -6\naddx -11\nnoop\nnoop\nnoop\n"

part2Tests :: TestTree
part2Tests = Tasty.testGroup "part 2 tests"
    [ HUnit.testCase "CRT" $
        Day10.fullScreenCRT '.' '#' exampleInstrs @?=
            "##..##..##..##..##..##..##..##..##..##..\n\
            \###...###...###...###...###...###...###.\n\
            \####....####....####....####....####....\n\
            \#####.....#####.....#####.....#####.....\n\
            \######......######......######......####\n\
            \#######.......#######.......#######.....\n" ]

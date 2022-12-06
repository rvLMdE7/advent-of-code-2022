{-# LANGUAGE OverloadedStrings #-}

module Test06 where

import Data.Text.Encoding qualified as Text.Enc
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Data.Text ()  -- needed to bring @IsString Text@ in scope
import Control.Arrow ((>>>))

import Day06 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [part1Tests]

part1Tests :: TestTree
part1Tests = Tasty.testGroup "part 1 tests"
    [ HUnit.testCase "main example" $
        markerEnd "mjqjpqmgbljsphdztnvjfqwrcgsmlb" @?= Just 7
    , HUnit.testCase "aux. example 1" $
        markerEnd "bvwbjplbgvbhsrlpgdmjqwftvncz" @?= Just 5
    , HUnit.testCase "aux. example 2" $
        markerEnd "nppdvjthqldpwncqszvftbrmjlhg" @?= Just 6
    , HUnit.testCase "aux. example 3" $
        markerEnd "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" @?= Just 10
    , HUnit.testCase "aux. example 4" $
        markerEnd "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" @?= Just 11 ]
  where
    markerEnd = Text.Enc.encodeUtf8 >>> Day06.endOfPacketMarker

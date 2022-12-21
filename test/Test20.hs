module Test20 where

import Data.Maybe (fromJust)
import Data.Sequence qualified as Seq
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Text.InterpolatedString.Perl6 (qq)

import Day20 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [part1Tests, part2Tests]

part1Tests :: TestTree
part1Tests = Tasty.testGroup "part 1 tests" $ do
    (n, (off, xs)) <- zip [0..] lists
    let ys = raw !! (n + 1)
    pure $ HUnit.testCase [qq|step $n|] $ mixAtIndex off xs @?= ys
  where
    raw = Seq.fromList <$>
        [ [1, 2, -3, 3, -2, 0, 4]
        , [2, 1, -3, 3, -2, 0, 4]
        , [1, -3, 2, 3, -2, 0, 4]
        , [1, 2, 3, -2, -3, 0, 4]
        , [1, 2, -2, -3, 0, 3, 4]
        , [1, 2, -3, 0, 3, 4, -2]
        , [1, 2, -3, 0, 3, 4, -2]
        , [1, 2, -3, 4, 0, 3, -2] ]
    lists = do
        (i, list) <- zip [0 .. length raw - 2] raw  -- zip & dropEnd 1
        let nextIndex = Seq.elemIndexL (head raw `Seq.index` i) list
        pure (fromJust nextIndex, list)
    mixAtIndex i xs = Day20.moveAtByLoop i (xs `Seq.index` i) xs

part2Tests :: TestTree
part2Tests = Tasty.testGroup "part 2 tests"
    [ HUnit.testCase "example" $
        Day20.part2 (Seq.fromList [1, 2, -3, 3, -2, 0, 4])
            @?= 1_623_178_306 ]

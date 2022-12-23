{-# LANGUAGE OverloadedStrings #-}

module Test07 where

import Data.Tree (Tree)
import Data.Tree qualified as Tree
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Text.Megaparsec qualified as Parse

import Day07 qualified
import Day07 (FileObj(..))


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [part1Tests, part2Tests]

exampleFileTree :: Tree FileObj
exampleFileTree =
    Tree.Node (Dir "/")
        [ Tree.Node (Dir "a")
            [ Tree.Node (Dir "e")
                [ Tree.Node (File 584 "i") [] ]
            , Tree.Node (File 29116 "f") []
            , Tree.Node (File 2557 "g") []
            , Tree.Node (File 62596 "h.lst") [] ]
        , Tree.Node (File 14848514 "b.txt") []
        , Tree.Node (File 8504156 "c.dat") []
        , Tree.Node (Dir "d")
            [ Tree.Node (File 4060174 "j") []
            , Tree.Node (File 8033020 "d.log") []
            , Tree.Node (File 5626152 "d.ext") []
            , Tree.Node (File 7214296 "k") [] ] ]

part1Tests :: TestTree
part1Tests = Tasty.testGroup "part 1 tests"
    [ HUnit.testCase "make filetree from cmd outputs" $
        fmap Day07.inferFileTree tryParse @?= Right exampleFileTree
    , HUnit.testCase "recursive dir sizes" $
        Day07.dirsSized (const True) exampleSized @?=
            [ ("/", 48381165)
            , ("a", 94853)
            , ("e", 584)
            , ("d", 24933642) ]
    , HUnit.testCase "dirs smaller than 100,000" $
        fmap fst (Day07.dirsSized (<= 100_000) exampleSized)
            @?= ["a", "e"] ]
  where
    exampleSized = Day07.annDirSizes exampleFileTree
    tryParse = Parse.runParser (Day07.parseInput <* Parse.eof) "day-07"
        "$ cd /\n\
        \$ ls\n\
        \dir a\n\
        \14848514 b.txt\n\
        \8504156 c.dat\n\
        \dir d\n\
        \$ cd a\n\
        \$ ls\n\
        \dir e\n\
        \29116 f\n\
        \2557 g\n\
        \62596 h.lst\n\
        \$ cd e\n\
        \$ ls\n\
        \584 i\n\
        \$ cd ..\n\
        \$ cd ..\n\
        \$ cd d\n\
        \$ ls\n\
        \4060174 j\n\
        \8033020 d.log\n\
        \5626152 d.ext\n\
        \7214296 k\n"

part2Tests :: TestTree
part2Tests = Tasty.testGroup "part 2 tests"
    [ HUnit.testCase "dirs to delete" $
        Day07.dirsToDelUpdate total needed exampleFileTree @?=
            [ ("d", 24933642)
            , ("/", 48381165) ] ]
  where
    total = 70_000_000
    needed = 30_000_000

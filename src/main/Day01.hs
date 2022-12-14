module Day01 where

import Control.Arrow ((>>>))
import Data.Ord (Down(Down), comparing)
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Data.Vector.Algorithms.Merge qualified as Vec.Merge
import System.Exit (die)
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.Char
import Text.Megaparsec.Char.Lexer qualified as Parse.Lex

import Common (Parser, readInputFileUtf8)


parseGroup :: Parser (Vector Word)
parseGroup = Vec.fromList <$> Parse.sepEndBy Parse.Lex.decimal Parse.Char.eol

parseInput :: Parser (Vector (Vector Word))
parseInput = Vec.fromList <$> Parse.sepEndBy parseGroup Parse.Char.eol

part1 :: Vector (Vector Word) -> Word
part1 = fmap Vec.sum >>> Vec.foldl' max 0

part2 :: Vector (Vector Word) -> Word
part2 = fmap Vec.sum
    >>> Vec.modify (Vec.Merge.sortBy $ comparing Down)
    >>> Vec.take 3
    >>> Vec.sum

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-01.txt"
    case Parse.runParser (parseInput <* Parse.eof) "day-01" text of
        Left err -> die $ Parse.errorBundlePretty err
        Right calories -> do
            print $ part1 calories
            print $ part2 calories

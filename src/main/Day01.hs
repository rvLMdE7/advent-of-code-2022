module Day01 where

import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Flow ((.>))
import System.Exit (die)
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.Char
import Text.Megaparsec.Char.Lexer qualified as Parse.Lex

import Common (Parser, readInputFileUtf8)


parseGroup :: Parser (Vector Word)
parseGroup = fmap Vec.fromList $
    Parse.Lex.decimal `Parse.sepEndBy` Parse.Char.eol

parseInput :: Parser (Vector (Vector Word))
parseInput = fmap Vec.fromList $
    parseGroup `Parse.sepEndBy` Parse.Char.eol

part1 :: Vector (Vector Word) -> Word
part1 = fmap Vec.sum .> Vec.foldl' max 0

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-01.txt"
    case Parse.runParser parseInput "day-01" text of
        Left err -> die $ Parse.errorBundlePretty err
        Right calories -> do
            print $ part1 calories

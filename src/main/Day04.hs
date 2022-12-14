{-# LANGUAGE RecordWildCards #-}

module Day04 where

import Control.Arrow ((>>>))
import Control.Monad (unless)
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import System.Exit (die)
import Text.InterpolatedString.Perl6 (qq)
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.Char
import Text.Megaparsec.Char.Lexer qualified as Parse.Char.Lex

import Common (Parser, readInputFileUtf8)


data Range = MkRange
    { begin :: Int
    , end :: Int }
    deriving (Eq, Ord, Show)

parseRange :: Parser Range
parseRange = do
    begin <- Parse.Char.Lex.decimal <* Parse.single '-'
    end <- Parse.Char.Lex.decimal
    unless (begin <= end) $ fail [qq|begin $begin greater than end $end|]
    pure $ MkRange{..}

parseRangePair :: Parser (Range, Range)
parseRangePair = do
    left <- parseRange <* Parse.single ','
    right <- parseRange
    pure (left, right)

parseRangePairs :: Parser (Vector (Range, Range))
parseRangePairs = Vec.fromList <$> Parse.sepEndBy parseRangePair Parse.Char.eol

fullyContains :: Range -> Range -> Bool
fullyContains big small = (begin big <= begin small) && (end small <= end big)

nested :: Range -> Range -> Bool
nested one two = (one `fullyContains` two) || (two `fullyContains` one)

overlap :: Range -> Range -> Bool
overlap one two = go one two || go two one
  where
    go a b = (begin b <= begin a) && (begin a <= end b)

part1 :: Vector (Range, Range) -> Int
part1 = Vec.filter (uncurry nested) >>> Vec.length

part2 :: Vector (Range, Range) -> Int
part2 = Vec.filter (uncurry overlap) >>> Vec.length

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-04.txt"
    case Parse.runParser (parseRangePairs  <* Parse.eof) "day-04" text of
        Left err -> die $ Parse.errorBundlePretty err
        Right rangePairs -> do
            print $ part1 rangePairs
            print $ part2 rangePairs

{-# LANGUAGE RecordWildCards #-}

module Day03 where

import Control.Applicative (some)
import Control.Arrow ((>>>))
import Control.Monad (when)
import Data.Char qualified as Char
import Data.HashSet qualified as HashSet
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import System.Exit (die)
import Text.InterpolatedString.Perl6 (qq)
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.Char

import Common (Parser, readInputFileUtf8, duomap)


data Rucksack = MkRucksack
    { firstComp :: Text
    , secondComp :: Text
    , shared :: Char }
    deriving (Eq, Ord, Show)

isAsciiLetter :: Char -> Bool
isAsciiLetter c = Char.isAscii c && Char.isLetter c

parseRucksack :: Parser Rucksack
parseRucksack = do
    rucksack <- fmap Text.pack $ some $ Parse.satisfy isAsciiLetter
    let len = Text.length rucksack
    when (odd len) $ fail [qq|rucksack contains odd number ($len) of items|]
    let both@(firstComp, secondComp) = Text.splitAt (len `div` 2) rucksack
    let (firstSet, secondSet) = duomap (Text.unpack >>> HashSet.fromList) both
    case HashSet.toList $ HashSet.intersection firstSet secondSet of
        [shared] -> pure $ MkRucksack{..}
        _        -> fail [qq|rucksack compartments lack unique shared item|]

parseRucksacks :: Parser (Vector Rucksack)
parseRucksacks = Vec.fromList <$> Parse.sepEndBy parseRucksack Parse.Char.eol

priority :: Char -> Int
priority c
    | Char.isAsciiLower c = Char.ord c - Char.ord 'a' + 1
    | Char.isAsciiUpper c = Char.ord c - Char.ord 'A' + 27
    | otherwise           = 0

part1 :: Vector Rucksack -> Int
part1 = Vec.map (shared >>> priority) >>> Vec.sum

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-03.txt"
    case Parse.runParser (parseRucksacks  <* Parse.eof) "day-03" text of
        Left err -> die $ Parse.errorBundlePretty err
        Right rucksacks -> do
            print $ part1 rucksacks

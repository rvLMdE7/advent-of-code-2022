{-# LANGUAGE RecordWildCards #-}

module Day03 where

import Control.Applicative (some)
import Control.Arrow ((>>>))
import Control.Monad (when)
import Data.Char qualified as Char
import Data.Foldable (for_)
import Data.Function ((&))
import Data.HashSet qualified as HashSet
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Data.Vector.Mutable qualified as Vec.Mut
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

-- | Takes every 3 consecutive elements, and bundles them up into a 3-tuple.
-- Any left-over elements at the end of the vector are dropped.
--
-- >>> bundle3 $ Vec.fromList [1..10]
-- [(1,2,3),(4,5,6),(7,8,9)]
bundle3 :: Vector a -> Vector (a, a, a)
bundle3 vec = Vec.create $ do
    mut <- Vec.Mut.new len
    for_ [0 .. len - 1] $ \i ->
        let index j = vec Vec.! (3*i + j)
        in  Vec.Mut.write mut i (index 0, index 1, index 2)
    pure mut
  where
    len = Vec.length vec `div` 3

badge :: (Rucksack, Rucksack, Rucksack) -> Maybe Char
badge (one, two, three) = case HashSet.toList common of
    [char] -> Just char
    _      -> Nothing
  where
    makeSet ruck =
        HashSet.fromList (Text.unpack $ firstComp ruck) `HashSet.union`
        HashSet.fromList (Text.unpack $ secondComp ruck)
    common =
        makeSet one `HashSet.intersection`
        makeSet two `HashSet.intersection`
        makeSet three

part2 :: Vector Rucksack -> Maybe Int
part2 = bundle3
    >>> Vec.map badge
    >>> sequence
    >>> fmap (fmap priority >>> Vec.sum)

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-03.txt"
    case Parse.runParser (parseRucksacks  <* Parse.eof) "day-03" text of
        Left err -> die $ Parse.errorBundlePretty err
        Right rucksacks -> do
            part1 rucksacks & print
            part2 rucksacks & maybe "error" show & putStrLn

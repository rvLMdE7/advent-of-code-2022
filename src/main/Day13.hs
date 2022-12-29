{-# LANGUAGE OverloadedStrings #-}

module Day13 where

import Control.Arrow ((>>>))
import Data.Foldable (asum)
import Data.List qualified as List
import Data.List.Extra (merge)
import Data.Maybe (fromJust)
import Data.Text (Text)
import System.Exit (die)
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.Char
import Text.Megaparsec.Char.Lexer qualified as Parse.Char.Lex

import Common (Parser, show', readInputFileUtf8)


newtype Input a = In
    { out :: [Either a (Input a)] }
    deriving (Eq, Show)

instance Ord a => Ord (Input a) where
    compare = order

order :: Ord a => Input a -> Input a -> Ordering
order (In [])     (In [])     = EQ
order (In [])     (In _)      = LT
order (In _)      (In [])     = GT
order (In (a:as)) (In (b:bs)) = ordering a b <> order (In as) (In bs)

ordering :: Ord a => Either a (Input a) -> Either a (Input a) -> Ordering
ordering (Left a)   (Left b)   = a `compare` b
ordering (Right as) (Right bs) = as `order` bs
ordering a@(Left _) (Right bs) = In [a] `order` bs
ordering (Right as) b@(Left _) = as `order` In [b]


parseInput :: Parser a -> Parser (Input a)
parseInput parser = do
    Parse.single '[' *> Parse.Char.hspace
    input <- flip Parse.sepBy comma $ asum
        [ Right <$> parseInput parser
        , Left <$> parser ]
    Parse.Char.hspace <* Parse.single ']'
    pure $ In input
  where
    comma = Parse.Char.hspace *> Parse.single ',' *> Parse.Char.hspace

parseInputPair :: Parser a -> Parser (Input a, Input a)
parseInputPair parser = do
    one <- parseInput parser <* Parse.Char.eol
    two <- parseInput parser <* Parse.Char.eol
    pure (one, two)

parseInputPairs :: Parser a -> Parser [(Input a, Input a)]
parseInputPairs parser = parseInputPair parser `Parse.sepEndBy` Parse.Char.eol

parseInputPairs' :: Parser [(Input Int, Input Int)]
parseInputPairs' = parseInputPairs Parse.Char.Lex.decimal


{-# ANN pretty ("HLINT: ignore" :: Text) #-}
pretty :: Show a => Input a -> Text
pretty = go >>> wrap
  where
    go = out >>> \case
        []            -> ""
        Left l : []   -> show' l
        Left l : xs   -> show' l <> "," <> go (In xs)
        Right rs : [] -> wrap (go rs)
        Right rs : xs -> wrap (go rs) <> "," <> go (In xs)
    wrap txt = "[" <> txt <> "]"


part1 :: Ord a => [(Input a, Input a)] -> Int
part1 = zip [1..] >>> filter (snd >>> lessThan) >>> fmap fst >>> sum
  where
    lessThan (a, b) = a < b

part2 :: [(Input Int, Input Int)] -> Int
part2 pairs = index (divider 2) * index (divider 6)
  where
    (ones, twos) = unzip pairs
    sorted =
        List.sort (divider 2 : ones) `merge`
        List.sort (divider 6 : twos)
    divider x = In [Right $ In [Left x]]
    index x = 1 + fromJust (List.elemIndex x sorted)

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-13.txt"
    case Parse.runParser (parseInputPairs' <* Parse.eof) "day-13" text of
        Left err -> die $ Parse.errorBundlePretty err
        Right pairs -> do
            print $ part1 pairs
            print $ part2 pairs

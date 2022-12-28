{-# LANGUAGE OverloadedStrings #-}

module Day13 where

import Control.Arrow ((>>>))
import Data.Foldable (asum)
import Data.Text (Text)
import System.Exit (die)
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.Char
import Text.Megaparsec.Char.Lexer qualified as Parse.Char.Lex

import Common (Parser, show', readInputFileUtf8)


newtype Input a = In
    { out :: [Either a (Input a)] }
    deriving (Eq, Show)


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


part1 :: Ord a => [(Input a, Input a)] -> Int
part1 = zip [1..] >>> filter (snd >>> inOrder) >>> fmap fst >>> sum
  where
    inOrder (a, b) = order a b == LT

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-13.txt"
    case Parse.runParser (parseInputPairs' <* Parse.eof) "day-13" text of
        Left err -> die $ Parse.errorBundlePretty err
        Right pairs -> do
            print $ part1 pairs

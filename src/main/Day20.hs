module Day20 where

import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.Maybe (fromJust)
import Data.Sequence (Seq((:<|), (:|>)))
import Data.Sequence qualified as Seq
import System.Exit (die)
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.Char
import Text.Megaparsec.Char.Lexer qualified as Parse.Char.Lex

import Common (Parser, readInputFileUtf8)


parseCoords :: Parser (Seq Int)
parseCoords = Seq.fromList <$> Parse.sepEndBy num Parse.Char.eol
  where
    num = Parse.Char.Lex.signed Parse.Char.hspace Parse.Char.Lex.decimal

infixl 7 `modulo`  -- same as `mod`
-- | The same as 'mod', except that where @x `mod` n@ would return @0@ this
-- function returns @n@ instead.
modulo :: Integral a => a -> a -> a
n `modulo` modulus = mod (n - 1) modulus + 1

indexLoop :: Seq a -> Int -> a
indexLoop list i = list `Seq.index` (i `mod` Seq.length list)

moveAtByLoop :: Int -> Int -> Seq a -> Seq a
moveAtByLoop at by list = moveAtBy at (absolute - at) list
  where
    target = at + by
    absolute = target `modulo` (Seq.length list - 1)

moveAtBy :: Int -> Int -> Seq a -> Seq a
moveAtBy at by list = case by `compare` 0 of
    LT -> case Seq.splitAt (at + 1) list of
        (ls :|> l, right) -> Seq.insertAt (Seq.length ls + by) l ls <> right
        (Seq.Empty, right) -> right
    GT -> case Seq.splitAt at list of
        (left, r :<| rs) -> left <> Seq.insertAt by r rs
        (left, Seq.Empty) -> left
    EQ -> list

mixN :: Int -> Seq (Int, Int) -> Seq Int
mixN n list = list & loop 0 0 & fmap snd
  where
    loop iter run
        | iter < len  = mix iter >>> loop (iter + 1) run
        | run + 1 < n = loop 0 (run + 1)
        | otherwise   = id
    mix iter prev = moveAtByLoop numIx num prev
      where
        numIx = fromJust $ Seq.findIndexL (fst >>> (==) iter) prev
        (_, num) = prev `Seq.index` numIx
    len = Seq.length list

part1 :: Seq Int -> Int
part1 list = sum $ do
    i <- [1000, 2000, 3000]
    pure $ mixed `indexLoop` (zeroIx + i)
  where
    indexed = Seq.fromFunction (Seq.length list) id `Seq.zip` list
    mixed = mixN 1 indexed
    zeroIx = fromJust $ Seq.elemIndexL 0 mixed

part2 :: Seq Int -> Int
part2 list = sum $ do
    i <- [1000, 2000, 3000]
    pure $ mixed `indexLoop` (zeroIx + i)
  where
    mul n = n * 811_589_153
    decrypted = Seq.fromFunction (Seq.length list) id `Seq.zip` (mul <$> list)
    mixed = mixN 10 decrypted
    zeroIx = fromJust $ Seq.elemIndexL 0 mixed

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-20.txt"
    case Parse.runParser (parseCoords <* Parse.eof) "day-20" text of
        Left err -> die $ Parse.errorBundlePretty err
        Right list -> do
            print $ part1 list
            print $ part2 list

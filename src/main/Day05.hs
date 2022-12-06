{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Day05 where

import Control.Applicative (optional, many)
import Control.Arrow ((>>>))
import Control.Monad (replicateM_, void)
import Control.Monad.ST (ST, runST)
import Data.Foldable (asum)
import Data.Function ((&))
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq((:<|)))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Data.Vector.Mutable (MVector)
import Data.Vector.Mutable qualified as Vec.Mut
import System.Exit (die)
import Text.InterpolatedString.Perl6 (qq)
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.Char
import Text.Megaparsec.Char.Lexer qualified as Parse.Char.Lex

import Common (Parser, isHSpace, readInputFileUtf8, show', (<<$>>))


type Chars = Seq Char  -- for brevity

singleHSpace :: Parser Char
singleHSpace = Parse.satisfy isHSpace

parseCrate :: Parser Char
parseCrate = Parse.single '[' *> Parse.anySingle <* Parse.single ']'

parseRow :: Parser [Maybe Char]
parseRow = entry `Parse.sepBy` singleHSpace
  where
    entry = asum
        [ Just <$> parseCrate
        , Nothing <$ replicateM_ 3 singleHSpace ]

{-# ANN parseRows ("HLint: ignore" :: Text) #-}
parseRows :: Parser (Vector Chars)
parseRows = do
    rows <- Parse.try parseRow `Parse.sepEndBy` Parse.Char.eol
    pure $ Vec.fromList $ fmap catMaybesSeq $ List.transpose rows
  where
    catMaybesSeq = Maybe.catMaybes >>> Seq.fromList

-- | We don't care about the indices, as they should be equal to [1..n] for
-- some n. So, this parser simply matches exactly that, in the expected
-- format, and then returns nothing.
--
-- Note: this implementation will likely break if n > 9.
parseIndices :: Parser ()
parseIndices = go 1
  where
    go :: Int -> Parser ()
    go n = void $ optional $ do
        void $ singleHSpace *> Parse.chunk (show' n) <* singleHSpace
        void $ optional $ singleHSpace *> go (n + 1)

-- | We decrement the from/to values by 1, so that they can be used right away
-- after parsing for 0-based indexing.
parseMove :: Parser (Int, Int, Int)
parseMove = do
    Parse.chunk "move" *> Parse.Char.hspace
    count <- Parse.Char.Lex.decimal <* Parse.Char.hspace
    Parse.chunk "from" *> Parse.Char.hspace
    from <- Parse.Char.Lex.decimal <* Parse.Char.hspace
    Parse.chunk "to" *> Parse.Char.hspace
    to <- Parse.Char.Lex.decimal <* Parse.Char.hspace
    pure (count, from - 1, to - 1)

parseMoves :: Parser (Vector (Int, Int, Int))
parseMoves = Vec.fromList <$> Parse.sepEndBy parseMove Parse.Char.eol

parseInput :: Parser (Vector Chars, Vector (Int, Int, Int))
parseInput = do
    stacks <- parseRows
    parseIndices <* many Parse.Char.eol
    moves <- parseMoves
    pure (stacks, moves)


move1 :: Int -> Int -> MVector s Chars -> ST s (Maybe Text)
move1 from to stacks
    | not (inBounds from) = pure $ Just [qq|index from=$from out-of-bounds|]
    | not (inBounds to) = pure $ Just [qq|index to=$to out-of-bounds|]
    | otherwise = Vec.Mut.unsafeRead stacks from >>= \case
        Seq.Empty -> pure $ Just [qq|nothing to move at index from=$from|]
        c :<| cs  -> do
            Vec.Mut.unsafeWrite stacks from cs
            Vec.Mut.unsafeModify stacks (c :<|) to
            pure Nothing
  where
    inBounds i = (0 <= i) && (i < Vec.Mut.length stacks)

moveN :: Int -> Int -> Int -> MVector s Chars -> ST s (Maybe Text)
moveN count from to stacks
    | count > 0 = move1 from to stacks >>= \case
        Nothing  -> moveN (count - 1) from to stacks
        Just err -> pure $ Just [qq|error with count=$count to-go: $err|]
    | otherwise = pure Nothing

-- | Like 'moveN', but preserves order when moving rather than reversing.
moveN' :: Int -> Int -> Int -> MVector s Chars -> ST s (Maybe Text)
moveN' count from to stacks
    | not (inBounds from) = pure $ Just [qq|index from=$from out-of-bounds|]
    | not (inBounds to) = pure $ Just [qq|index to=$to out-of-bounds|]
    | count <= 0 = pure Nothing
    | otherwise = do
        (transfer, keep) <- Seq.splitAt count <$> Vec.Mut.read stacks from
        if length transfer == count
            then do
                Vec.Mut.write stacks from keep
                Vec.Mut.modify stacks (transfer <>) to
                pure Nothing
            else pure $ Just [qq|not enough values at index from=$from|]
  where
    inBounds i = (0 <= i) && (i < Vec.Mut.length stacks)

applyST
    :: (Int -> Int -> Int -> MVector s Chars -> ST s (Maybe Text))
    -> Vector (Int, Int, Int)
    -> MVector s Chars
    -> ST s (Maybe Text)
applyST move moves stacks = Vec.ifoldM' go Nothing moves
  where
    go err i (count, from, to) = case err of
        Just _  -> pure err
        Nothing ->
            let msg e = [qq|error on move $i: $e|]
            in  msg <<$>> move count from to stacks

apply
    :: (forall s. Int -> Int -> Int -> MVector s Chars -> ST s (Maybe Text))
    -> Vector (Int, Int, Int)
    -> Vector Chars
    -> Either Text (Vector Chars)
apply move moves stacks = runST $ do
    mut <- Vec.thaw stacks
    applyST move moves mut >>= \case
        Nothing  -> Right <$> Vec.freeze mut
        Just err -> pure $ Left err

topOfStacks :: Vector Chars -> Either Text (Vector Char)
topOfStacks = traverse listToEither
  where
    listToEither = \case
        Seq.Empty -> Left "can't get top of empty stack"
        c :<| _   -> Right c


part1 :: Vector (Int, Int, Int) -> Vector Chars -> Either Text (Vector Char)
part1 moves stacks = apply moveN moves stacks >>= topOfStacks

part2 :: Vector (Int, Int, Int) -> Vector Chars -> Either Text (Vector Char)
part2 moves stacks = apply moveN' moves stacks >>= topOfStacks

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-05.txt"
    case Parse.runParser (parseInput <* Parse.eof) "day-05" text of
        Left err -> die $ Parse.errorBundlePretty err
        Right (stacks, moves) -> do
            part1 moves stacks
                & either id (Vec.toList >>> Text.pack)
                & Text.IO.putStrLn
            part2 moves stacks
                & either id (Vec.toList >>> Text.pack)
                & Text.IO.putStrLn

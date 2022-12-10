{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module Day09 where

import Control.Arrow ((>>>))
import Control.Monad (replicateM_)
import Control.Monad.Trans.State (State, execState)
import Data.Foldable (asum, for_)
import Data.Functor (($>))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import GHC.Generics (Generic)
import Linear (V2(V2))
import Optics (use, _last, preuse, ix, atraversal, view, set, (%))
import Optics.State.Operators ((%=))
import System.Exit (die)
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.Char
import Text.Megaparsec.Char.Lexer qualified as Parse.Char.Lex

import Common (Parser, readInputFileUtf8, (+=))


data Direction = RightDir | UpDir | LeftDir | DownDir
    deriving (Eq, Ord, Show)

data Move = MkMove
    { direction :: Direction
    , distance :: Int }
    deriving (Eq, Ord, Show)

data Knot = MkKnot
    { headPos :: V2 Int
    , bodyPos :: Vector (V2 Int)
    , tailGhost :: Set (V2 Int) }
    deriving (Eq, Generic, Ord, Show)

parseDirection :: Parser Direction
parseDirection = asum
    [ Parse.single 'R' $> RightDir
    , Parse.single 'U' $> UpDir
    , Parse.single 'L' $> LeftDir
    , Parse.single 'D' $> DownDir ]

parseMove :: Parser Move
parseMove = do
    direction <- parseDirection <* Parse.Char.hspace
    distance <- Parse.Char.Lex.decimal
    pure $ MkMove{..}

parseMoves :: Parser (Vector Move)
parseMoves = Vec.fromList <$> Parse.sepEndBy parseMove Parse.Char.eol

dirToVec :: Direction -> V2 Int
dirToVec = \case
    RightDir -> V2 1 0
    UpDir    -> V2 0 1
    LeftDir  -> V2 (-1) 0
    DownDir  -> V2 0 (-1)

applyDir :: Direction -> State Knot ()
applyDir dir = do
    #headPos += dirToVec dir

    len <- Vec.length <$> use #bodyPos
    let headPosAff = atraversal (Right . view #headPos) (flip $ set #headPos)
        base = [ #bodyPos % ix i | i <- [0 .. len-1] ]
        sliding = zip (headPosAff : base) base

    for_ sliding $ \(one, two) -> do
        one' <- preuse one
        two' <- preuse two
        case (-) <$> one' <*> two' of
            Nothing    -> pure ()
            Just delta -> case delta of
                -- straight
                V2   2    0  -> two += V2   1    0
                V2   0    2  -> two += V2   0    1
                V2 (-2)   0  -> two += V2 (-1)   0
                V2   0  (-2) -> two += V2   0  (-1)

                -- diagonal
                V2   2    1  -> two += V2   1    1
                V2   2  (-1) -> two += V2   1  (-1)
                V2 (-2)   1  -> two += V2 (-1)   1
                V2 (-2) (-1) -> two += V2 (-1) (-1)
                V2   1    2  -> two += V2   1    1
                V2 (-1)   2  -> two += V2 (-1)   1
                V2   1  (-2) -> two += V2   1  (-1)
                V2 (-1) (-2) -> two += V2 (-1) (-1)

                -- within 1 tile
                _ -> pure ()

    preuse (#bodyPos % _last) >>= \case
        Nothing  -> pure ()
        Just new -> #tailGhost %= Set.insert new

applyMove :: Move -> State Knot ()
applyMove MkMove{..} = replicateM_ distance (applyDir direction)

applyMoves :: Vector Move -> State Knot ()
applyMoves = Vec.foldM'_ (const applyMove) ()

runMoves :: Int -> Vector Move -> Knot
runMoves n = applyMoves >>> flip execState initState
  where
    initState = MkKnot
        { headPos = V2 0 0
        , bodyPos = Vec.replicate n $ V2 0 0
        , tailGhost = Set.singleton $ V2 0 0 }

part1 :: Vector Move -> Int
part1 = runMoves 1 >>> tailGhost >>> Set.size

part2 :: Vector Move -> Int
part2 = runMoves 10 >>> tailGhost >>> Set.size

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-09.txt"
    case Parse.runParser (parseMoves <* Parse.eof) "day-09" text of
        Left err -> die $ Parse.errorBundlePretty err
        Right moves -> do
            print $ part1 moves
            print $ part2 moves

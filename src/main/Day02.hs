{-# LANGUAGE RecordWildCards #-}

module Day02 where

import Control.Arrow ((>>>))
import Data.Foldable (asum)
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import System.Exit (die)
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.Char

import Common (Parser, readInputFileUtf8)


data Opponent = A | B | C
    deriving (Bounded, Enum, Eq, Ord, Show)

data Player = X | Y | Z
    deriving (Bounded, Enum, Eq, Ord, Show)

data Generic o p = MkGeneric
    { opponent :: o
    , player :: p }
    deriving (Eq, Ord, Show)

data Move = Rock | Paper | Scissors
    deriving (Bounded, Enum, Eq, Ord, Show)

type Encrypted = Generic Opponent Player

type Round = Generic Move Move

data Outcome = OpponentWin | Draw | PlayerWin
    deriving (Bounded, Enum, Eq, Ord, Show)

parseOpponent :: Parser Opponent
parseOpponent = asum
    [ A <$ Parse.single 'A'
    , B <$ Parse.single 'B'
    , C <$ Parse.single 'C' ]

parsePlayer :: Parser Player
parsePlayer = asum
    [ X <$ Parse.single 'X'
    , Y <$ Parse.single 'Y'
    , Z <$ Parse.single 'Z' ]

parseRound :: Parser Encrypted
parseRound = do
    opponent <- parseOpponent <* Parse.Char.hspace
    player <- parsePlayer
    pure $ MkGeneric{..}

parseGuide :: Parser (Vector Encrypted)
parseGuide = Vec.fromList <$> Parse.sepEndBy parseRound Parse.Char.eol

-- | Thinking of each move as being in Z mod 3, we calculate whether the
-- player's move is equal to:
--
--   * opponent's move + 1 => player win
--   * opponent's move     => draw
--   * opponent's move - 1 => opponent win
outcome :: Round -> Outcome
outcome MkGeneric{..} = case shifted `compare` 0 of
    GT -> PlayerWin
    EQ -> Draw
    LT -> OpponentWin
  where
    diff = fromEnum player - fromEnum opponent
    -- we do (+ 1) >>> (`mod` 3) >>> (- 1) to get diff as
    -- one of {-1, 0, 1} mod 3.
    shifted = mod (diff + 1) 3 - 1

scoreMove :: Move -> Int
scoreMove move = fromEnum move + 1

scoreOutcome :: Outcome -> Int
scoreOutcome out = fromEnum out * 3

scoreRound :: Round -> Int
scoreRound rnd = scoreMove (player rnd) + scoreOutcome (outcome rnd)

decrypt1 :: Encrypted -> Round
decrypt1 enc = MkGeneric
    { player = toEnum $ fromEnum $ player enc
    , opponent = toEnum $ fromEnum $ opponent enc }

decrypt2 :: Encrypted -> Round
decrypt2 enc = MkGeneric
    { player = toEnum $ case player enc of
        X -> (opp - 1) `mod` 3  -- lose
        Y -> opp                -- draw
        Z -> (opp + 1) `mod` 3  -- win
    , opponent = toEnum opp }
  where
    opp = fromEnum $ opponent enc

part1 :: Vector Encrypted -> Int
part1 = Vec.map (decrypt1 >>> scoreRound) >>> Vec.sum

part2 :: Vector Encrypted -> Int
part2 = Vec.map (decrypt2 >>> scoreRound) >>> Vec.sum

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-02.txt"
    case Parse.runParser (parseGuide  <* Parse.eof) "day-02" text of
        Left err -> die $ Parse.errorBundlePretty err
        Right guide -> do
            print $ part1 guide
            print $ part2 guide

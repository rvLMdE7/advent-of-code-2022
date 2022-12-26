{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Day10 where

import Control.Applicative ((<|>))
import Control.Arrow ((>>>), (&&&), (<<<))
import Control.Monad (when, unless)
import Control.Monad.State (State, gets, execState, put, evalState)
import Data.Function ((&))
import Data.List.Split (chunksOf)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import GHC.Generics (Generic)
import Optics.State.Operators ((.=))
import System.Exit (die)
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.Char
import Text.Megaparsec.Char.Lexer qualified as Parse.Char.Lex

import Common (Parser, (+=), readInputFileUtf8, cons)


data Instr = Noop | Addx Int
    deriving (Eq, Ord, Show)

data Cpu = MkCpu
    { cycleNum :: Int
    , regValue :: Int
    , remInstrs :: [Instr] }
    deriving (Eq, Generic, Ord, Show)

parseNoop :: Parser Instr
parseNoop = Noop <$ Parse.chunk "noop"

parseAddX :: Parser Instr
parseAddX = do
    Parse.chunk "addx" *> Parse.Char.hspace
    Addx <$> Parse.Char.Lex.signed Parse.Char.hspace Parse.Char.Lex.decimal

parseInstr :: Parser Instr
parseInstr = parseNoop <|> parseAddX

parseInstrs :: Parser [Instr]
parseInstrs = Parse.sepEndBy parseInstr Parse.Char.eol

evalInstr :: State Cpu ()
evalInstr = gets remInstrs >>= \case
    Noop : instrs -> do
        #cycleNum += 1
        #remInstrs .= instrs
    Addx v : instrs -> do
        #cycleNum += 2
        #regValue += v
        #remInstrs .= instrs
    [] -> pure ()

runWhile :: (Cpu -> Bool) -> State Cpu ()
runWhile test = do
    noInstrs <- gets $ remInstrs >>> null
    unless noInstrs $ do
        next <- gets $ execState evalInstr
        when (test next) $ do
            put next
            runWhile test

signalStrengthsAt :: [Int] -> State Cpu [Int]
signalStrengthsAt = \case
    []     -> pure []
    n : ns -> do
        runWhile $ \cpu -> cycleNum cpu <= n
        val <- gets regValue
        cons (val * n) <$> signalStrengthsAt ns

pixelChar :: Int -> Int -> Char
pixelChar num value =
    if abs (value - mod (num - 1) 40) < 2
        then '█'
        else ' '

runScreen :: State Cpu String
runScreen = do
    (val, num) <- gets (regValue &&& cycleNum)
    let char1 = pixelChar num val
    let char2 = pixelChar (num + 1) val
    gets remInstrs >>= \case
        Addx _ : _ -> do
            evalInstr
            fmap (cons char1 <<< cons char2) runScreen
        Noop : _ -> do
            evalInstr
            fmap (cons char1) runScreen
        [] -> pure ""

part1 :: [Instr] -> Int
part1 = MkCpu 1 1 >>> evalState prog >>> sum
  where
    prog = signalStrengthsAt [20, 60 .. 220]

part2 :: [Instr] -> Text
part2 = MkCpu 1 1
    >>> evalState runScreen
    >>> chunksOf 40
    >>> fmap Text.pack
    >>> Text.unlines

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-10.txt"
    case Parse.runParser (parseInstrs <* Parse.eof) "day-10" text of
        Left err -> die $ Parse.errorBundlePretty err
        Right instrs -> do
            part1 instrs & print
            part2 instrs & Text.IO.putStr

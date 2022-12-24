{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Day10 where

import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Control.Monad (when, unless)
import Control.Monad.State (State, gets, execState, put, evalState)
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

part1 :: [Instr] -> Int
part1 instrs = sum $ flip evalState (MkCpu 1 1 instrs) $
    signalStrengthsAt [20, 60 .. 220]

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-10.txt"
    case Parse.runParser (parseInstrs <* Parse.eof) "day-10" text of
        Left err -> die $ Parse.errorBundlePretty err
        Right instrs -> do
            print $ part1 instrs

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day11 where

import Control.Arrow ((>>>), (&&&))
import Control.Monad (void)
import Control.Monad.State (State)
import Data.Foldable (asum)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Optics (use, _1)
import System.Exit (die)
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.C
import Text.Megaparsec.Char.Lexer qualified as Parse.CL

import Common (Parser, readInputFileUtf8)


data Monkey = MkMonkey
    { number :: Int
    , items :: [Int]
    , operation :: Int -> Int
    , throwTo :: Int -> Int }

chunks :: Text -> Parser ()
chunks = Text.words
    >>> fmap (Parse.chunk >>> void)
    >>> List.intersperse Parse.C.hspace
    >>> sequence_

parseNumber :: Parser Int
parseNumber = do
    Parse.chunk "Monkey" *> Parse.C.hspace
    Parse.CL.decimal <* Parse.single ':'

parseItems :: Parser [Int]
parseItems = do
    Parse.C.hspace *> chunks "Starting items:" *> Parse.C.hspace
    Parse.sepBy Parse.CL.decimal $ Parse.single ',' *> Parse.C.hspace

parseOperation :: Parser (Int -> Int)
parseOperation =
    Parse.C.hspace *> chunks "Operation: new =" *> Parse.C.hspace *> asum
        [ Parse.try $ do
            Parse.chunk "old" *> Parse.C.hspace
            op <- parseOp <* Parse.C.hspace <* Parse.chunk "old"
            pure $ \old -> old `op` old
        , Parse.try $ do
            left <- Parse.CL.decimal <* Parse.C.hspace
            op <- parseOp <* Parse.C.hspace <* Parse.chunk "old"
            pure $ \old -> left `op` old
        , Parse.try $ do
            op <- Parse.chunk "old" *> Parse.C.hspace *> parseOp
            right <- Parse.C.hspace *> Parse.CL.decimal <* Parse.C.hspace
            pure $ \old -> old `op` right ]
  where
    parseOp = asum
        [ (*) <$ Parse.single '*'
        , (+) <$ Parse.single '+' ]

parseThrowTo :: Parser (Int -> Int)
parseThrowTo = do
    Parse.C.hspace *> chunks "Test: divisible by"
    quotient <- Parse.C.hspace *> Parse.CL.decimal <* Parse.C.eol
    Parse.C.hspace *> chunks "If true: throw to monkey"
    true <- Parse.C.hspace *> Parse.CL.decimal <* Parse.C.eol
    Parse.C.hspace *> chunks "If false: throw to monkey"
    false <- Parse.C.hspace *> Parse.CL.decimal
    pure $ \n -> if n `mod` quotient == 0 then true else false

parseMonkey :: Parser Monkey
parseMonkey = do
    number <- parseNumber <* Parse.C.eol
    items <- parseItems <* Parse.C.eol
    operation <- parseOperation <* Parse.C.eol
    throwTo <- parseThrowTo <* Parse.C.eol
    pure $ MkMonkey{..}

parseMonkeys :: Parser (IntMap Monkey)
parseMonkeys = mkMap <$> Parse.sepEndBy parseMonkey Parse.C.eol
  where
    mkMap = fmap (number &&& id) >>> IntMap.fromList

turn :: State (Int, IntMap Monkey) ()
turn = do
    n <- use _1
    pure ()

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-11.txt"
    case Parse.runParser (parseMonkeys <* Parse.eof) "day-11" text of
        Left err -> die $ Parse.errorBundlePretty err
        Right monkeys -> do
            pure ()

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day11 where

import Control.Arrow ((>>>), (&&&))
import Control.Monad (void, replicateM_)
import Control.Monad.State (State, gets, execState)
import Data.Foldable (asum, for_, traverse_)
import Data.Foldable qualified as Fold
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List qualified as List
import Data.Ord (Down(Down))
import Data.Sequence (Seq((:|>)))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Optics ((%), (&), use, at, mapped)
import Optics.State.Operators ((.=), (%=))
import System.Exit (die)
import Text.InterpolatedString.Perl6 (qq)
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.C
import Text.Megaparsec.Char.Lexer qualified as Parse.CL

import Common (Parser, (+=), readInputFileUtf8, show')
import Prelude hiding (round)


data Monkey = MkMonkey
    { number :: Int
    , items :: Seq Int
    , operation :: Int -> Int
    , quotient :: Int
    , true :: Int
    , false :: Int
    , inspected :: Int }
    deriving (Generic)

chunks :: Text -> Parser ()
chunks = Text.words
    >>> fmap (Parse.chunk >>> void)
    >>> List.intersperse Parse.C.hspace
    >>> sequence_

parseNumber :: Parser Int
parseNumber = do
    Parse.chunk "Monkey" *> Parse.C.hspace
    Parse.CL.decimal <* Parse.single ':'

parseItems :: Parser (Seq Int)
parseItems = do
    Parse.C.hspace *> chunks "Starting items:" *> Parse.C.hspace
    let comma = Parse.single ',' *> Parse.C.hspace
    Seq.fromList <$> Parse.sepBy Parse.CL.decimal comma

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

parseThrowTo :: Parser (Int, Int, Int)
parseThrowTo = do
    Parse.C.hspace *> chunks "Test: divisible by"
    quotient <- Parse.C.hspace *> Parse.CL.decimal <* Parse.C.eol
    Parse.C.hspace *> chunks "If true: throw to monkey"
    true <- Parse.C.hspace *> Parse.CL.decimal <* Parse.C.eol
    Parse.C.hspace *> chunks "If false: throw to monkey"
    false <- Parse.C.hspace *> Parse.CL.decimal
    pure (quotient, true, false)

parseMonkey :: Parser Monkey
parseMonkey = do
    number <- parseNumber <* Parse.C.eol
    items <- parseItems <* Parse.C.eol
    operation <- parseOperation <* Parse.C.eol
    (quotient, true, false) <- parseThrowTo <* Parse.C.eol
    pure $ let inspected = 0 in MkMonkey{..}

parseMonkeys :: Parser (IntMap Monkey)
parseMonkeys = mkMap <$> Parse.sepEndBy parseMonkey Parse.C.eol
  where
    mkMap = fmap (number &&& id) >>> IntMap.fromList

turn :: (Int -> Int) -> Int -> State (IntMap Monkey) ()
turn relief n = use (at n) >>= \case
    Nothing -> pure ()
    Just MkMonkey{..} -> do
        at n % mapped % #items .= Seq.Empty
        at n % mapped % #inspected += Seq.length items
        for_ items $ \item -> do
            let worry = relief $ operation item
            let dest = if worry `mod` quotient == 0 then true else false
            at dest % mapped % #items %= (:|> worry)

round :: (Int -> Int) -> State (IntMap Monkey) ()
round relief = gets IntMap.keys >>= traverse_ (turn relief)

showMonkeys :: IntMap Monkey -> Text
showMonkeys assoc = Text.unlines $ do
    (n, MkMonkey{..}) <- IntMap.toAscList assoc
    let count = Text.justifyRight 3 ' ' $ show' inspected
    pure $ mappend [qq|Monkey $n: inspected $count - |] $
        Text.intercalate ", " $ do
            item <- Fold.toList items
            pure $ show' item

part1 :: IntMap Monkey -> Int
part1 = execState prog
    >>> IntMap.elems
    >>> fmap inspected
    >>> List.sortOn Down
    >>> take 2
    >>> product
  where
    prog = replicateM_ 20 $ round $ flip div 3

{-# ANN part2 ("hlint: ignore" :: Text) #-}
part2 :: IntMap Monkey -> Int
part2 monkeys = monkeys
    & execState prog
    & IntMap.elems
    & fmap inspected
    & List.sortOn Down
    & take 2
    & product
  where
    prog = replicateM_ 10_000 $ round $ flip mod modulus
    modulus = product $ fmap quotient $ IntMap.elems monkeys

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-11.txt"
    case Parse.runParser (parseMonkeys <* Parse.eof) "day-11" text of
        Left err -> die $ Parse.errorBundlePretty err
        Right monkeys -> do
            print $ part1 monkeys
            print $ part2 monkeys

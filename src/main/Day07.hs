{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Day07 where

import Control.Arrow ((>>>))
import Data.Char qualified as Char
import Data.Foldable (asum)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Tree (Tree, Forest)
import Data.Tree qualified as Tree
import Optics ((&), (%~), (%), ix)
import System.Exit (die)
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.Char

import Common (Parser, readInputFileUtf8)


data CdEntry
    = InOne Text
    | OutOne
    | OutAll
    deriving (Eq, Ord, Show)

data LsEntry = LsDir Text | LsFile Int Text
    deriving (Eq, Ord, Show)

data Object = Dir Text | File Int Text
    deriving (Eq, Ord, Show)


parseWord :: Parser Text
parseWord = fmap Text.pack $ Parse.some $ Parse.satisfy $ not . Char.isSpace

parseCd :: Parser CdEntry
parseCd = Parse.chunk "cd" *> Parse.Char.hspace *> asum
    [ OutAll <$ Parse.single '/'
    , OutOne <$ Parse.chunk ".."
    , InOne <$> parseWord ]

parseLsDir :: Parser LsEntry
parseLsDir = do
    Parse.chunk "dir" *> Parse.Char.hspace
    LsDir <$> parseWord

parseLsFile :: Parser LsEntry
parseLsFile = do
    size <- Parse.some Parse.Char.digitChar
    name <- Parse.Char.hspace *> parseWord
    pure $ LsFile (read size) name

parseLs :: Parser [LsEntry]
parseLs = do
    Parse.chunk "ls" *> Parse.Char.hspace
    Parse.many $ Parse.try $ Parse.Char.eol *> asum
        [ parseLsFile
        , parseLsDir ]

parseInput :: Parser [Either [LsEntry] CdEntry]
parseInput = flip Parse.sepEndBy Parse.Char.eol $
    Parse.single '$' *> Parse.Char.hspace *> asum
        [ Left <$> parseLs
        , Right <$> parseCd ]

deduceFileSystem :: [Either [LsEntry] CdEntry] -> Tree Object
deduceFileSystem = go (Tree.Node (Dir "/") []) []
  where
    go :: Tree Object -> [Text] -> [Either [LsEntry] CdEntry] -> Tree Object
    go acc cwd = \case
        [] -> acc
        Left contents : instrs ->
            let new = update (Dir <$> reverse cwd) (insert contents) acc
            in  go new cwd instrs
        Right (InOne dir) : instrs -> go acc (dir : cwd) instrs
        Right OutOne : instrs -> go acc (drop 1 cwd) instrs
        Right OutAll : instrs -> go acc [] instrs

    insert :: [LsEntry] -> Forest Object -> Forest Object
    insert contents forest = List.foldr (flip insertOne) forest contents

    insertOne :: Forest Object -> LsEntry -> Forest Object
    insertOne forest = \case
        LsFile size name ->
            let seek tr = Tree.rootLabel tr == File size name
            in  case List.findIndex seek forest of
                    Nothing -> Tree.Node (File size name) [] : forest
                    Just _  -> forest
        LsDir name ->
            let seek tr = Tree.rootLabel tr == Dir name
            in  case List.findIndex seek forest of
                    Nothing -> Tree.Node (Dir name) [] : forest
                    Just _  -> forest

update :: Eq a => [a] -> (Forest a -> Forest a) -> Tree a -> Tree a
update path f tree = case path of
    []     -> tree & #subForest %~ f
    p : ps ->
        let seek tr = Tree.rootLabel tr == p
        in  case List.findIndex seek (Tree.subForest tree) of
                Nothing ->
                    let new = Tree.Node p []
                    in  tree & #subForest %~ (update ps f new :)
                Just i -> tree & #subForest % ix i %~ update ps f

printTree :: Show a => Tree a -> IO ()
printTree = fmap show >>> Tree.drawTree >>> putStrLn

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-07.txt"
    case Parse.runParser (parseInput <* Parse.eof) "day-07" text of
        Left err -> die $ Parse.errorBundlePretty err
        Right moves -> do
            printTree $ deduceFileSystem moves

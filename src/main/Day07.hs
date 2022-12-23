{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Day07 where

import Control.Applicative ((<|>), many, some)
import Control.Arrow ((>>>))
import Data.Char qualified as Char
import Data.Foldable (asum)
import Data.Functor (void)
import Data.List qualified as List
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq((:<|), (:|>)))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Tree (Tree, Forest)
import Data.Tree qualified as Tree
import Optics ((&), (%~), (%), ix)
import System.Exit (die)
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.Char

import Common (Parser, readInputFileUtf8, cons)


data CdEntry
    = InOne Text
    | OutOne
    | OutAll
    deriving (Eq, Ord, Show)

data LsEntry = LsDir Text | LsFile Int Text
    deriving (Eq, Ord, Show)

data FileObj = Dir Text | File Int Text
    deriving (Eq, Ord, Show)


parseWord :: Parser Text
parseWord = fmap Text.pack $ some $ Parse.satisfy $ not . Char.isSpace

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
    size <- some Parse.Char.digitChar
    name <- Parse.Char.hspace *> parseWord
    pure $ LsFile (read size) name

parseLs :: Parser [LsEntry]
parseLs = do
    Parse.chunk "ls" *> Parse.Char.hspace
    many $ Parse.try $ do
        void Parse.Char.eol
        parseLsFile <|> parseLsDir

parseInput :: Parser [Either [LsEntry] CdEntry]
parseInput = flip Parse.sepEndBy Parse.Char.eol $
    Parse.single '$' *> Parse.Char.hspace *> asum
        [ Left  <$> parseLs
        , Right <$> parseCd ]


inferFileTree :: [Either [LsEntry] CdEntry] -> Tree FileObj
inferFileTree = go Seq.Empty $ Tree.Node (Dir "/") []
  where
    go cwd acc = \case
        [] -> acc
        Left lss : entries ->
            let new = adjust (Dir <$> cwd) (insert lss) acc
            in  go cwd new entries
        Right cd : entries -> go (update cwd cd) acc entries
    update cwd = \case
        OutOne -> dropEnd 1 cwd
        OutAll -> Seq.Empty
        InOne dir -> cwd :|> dir
    insertOne ls forest = case List.findIndex seek forest of
        Nothing -> Tree.Node new [] : forest
        Just _  -> forest
      where
        seek tree = Tree.rootLabel tree == new
        new = case ls of
            LsDir name -> Dir name
            LsFile size name -> File size name
    insert = flip (List.foldr insertOne)

dropEnd :: Int -> Seq a -> Seq a
dropEnd n
    | n > 0 = \case
        list :|> _ -> dropEnd (n - 1) list
        list       -> list
    | otherwise = id

adjust :: Eq a => Seq a -> (Forest a -> Forest a) -> Tree a -> Tree a
adjust path f tree = case path of
    Seq.Empty -> tree & #subForest %~ f
    dir :<| dirs -> case List.findIndex (seek dir) (Tree.subForest tree) of
        Just i  -> tree & #subForest % ix i %~ adjust dirs f
        Nothing ->
            let new = Tree.Node dir []
            in  tree & #subForest %~ cons (adjust dirs f new)
  where
    seek dir tree' = Tree.rootLabel tree' == dir


annDirSizes :: Tree FileObj -> Tree (Int, FileObj)
annDirSizes = Tree.foldTree $ \obj subObjs ->
    let size = fmap (Tree.rootLabel >>> fst) >>> sum
    in  Tree.Node (fileSize obj + size subObjs, obj) subObjs

fileSize :: FileObj -> Int
fileSize = \case
    Dir _       -> 0
    File size _ -> size

dirsWithSize :: (Int -> Bool) -> Tree FileObj -> [(Text, Int)]
dirsWithSize f = annDirSizes >>> Tree.flatten >>> mapMaybe test
  where
    test = \case
        (size, Dir name) | f size -> Just (name, size)
        _                         -> Nothing


showTree' :: Show a => Tree a -> Text
showTree' = fmap show >>> Tree.drawTree >>> Text.pack

part1 :: Tree FileObj -> Int
part1 = dirsWithSize (<= 100_000) >>> fmap snd >>> sum

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-07.txt"
    case Parse.runParser (parseInput <* Parse.eof) "day-07" text of
        Left err -> die $ Parse.errorBundlePretty err
        Right inputs -> do
            let fileTree = inferFileTree inputs
            print $ part1 fileTree

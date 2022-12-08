module Day08 where

import Control.Applicative (some)
import Control.Arrow ((>>>))
import Control.Monad (guard)
import Data.Char qualified as Char
import Data.Containers.ListUtils (nubInt)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import System.Exit (die)
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.Char

import Common (Parser, readInputFileUtf8)


type Matrix a = Vector (Vector a)

parseRow :: Parser (Vector Int)
parseRow = Vec.fromList . fmap Char.digitToInt <$> some Parse.Char.digitChar

parseMatrix :: Parser (Matrix Int)
parseMatrix = do
    matrix <- Vec.fromList <$> Parse.sepEndBy parseRow Parse.Char.eol
    case nubInt $ Vec.toList $ Vec.map Vec.length matrix of
        _ : _ : _ -> fail "not a rectangular matrix"
        _         -> pure matrix

-- | @slice i j vector@ yields the slice of elements indexed [i .. j-1] in the
-- given vector.
slice :: Int -> Int -> Vector a -> Vector a
slice i j = Vec.slice i (max (j - i) 0)

visibleHoriz :: Matrix Int -> Set (Int, Int)
visibleHoriz matrix = Set.unions $ do
    y <- [0 .. height - 1]
    let row = matrix Vec.! y
    pure $ Set.fromList $ do
        x <- [0 .. width - 1]
        let value = row Vec.! x
        let lefts = slice 0 x row
        let rights = slice (x + 1) width row
        guard $ Vec.all (< value) lefts || Vec.all (< value) rights
        pure (x, y)
  where
    height = Vec.length matrix
    width = maybe 0 Vec.length (matrix Vec.!? 0)

visibleVert :: Matrix Int -> Set (Int, Int)
visibleVert matrix = Set.unions $ do
    x <- [0 .. width - 1]
    let col = Vec.map (Vec.! x) matrix
    pure $ Set.fromList $ do
        y <- [0 .. height - 1]
        let value = col Vec.! y
        let ups = slice 0 y col
        let downs = slice (y + 1) height col
        guard $ Vec.all (< value) ups || Vec.all (< value) downs
        pure (x, y)
  where
    height = Vec.length matrix
    width = maybe 0 Vec.length (matrix Vec.!? 0)

visible :: Matrix Int -> Set (Int, Int)
visible matrix = visibleHoriz matrix `Set.union` visibleVert matrix

part1 :: Matrix Int -> Int
part1 = visible >>> Set.size

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-08.txt"
    case Parse.runParser (parseMatrix <* Parse.eof) "day-08" text of
        Left err -> die $ Parse.errorBundlePretty err
        Right matrix -> do
            print $ part1 matrix

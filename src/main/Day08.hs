module Day08 where

import Control.Applicative (some)
import Control.Arrow ((>>>))
import Control.Monad (guard)
import Data.Char qualified as Char
import Data.Containers.ListUtils (nubInt)
import Data.Function ((&))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Linear (V2(V2), V4(V4))
import System.Exit (die)
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.Char

import Common (Parser, readInputFileUtf8)


type Matrix a = Vector (Vector a)

-- | Same as 'takeWhile', except that it also takes the first element (if any)
-- which violates the given predicate.
takeWhilst :: (a -> Bool) -> [a] -> [a]
takeWhilst test = \case
    x : xs -> x : if test x then takeWhilst test xs else []
    xs     -> xs

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

visibleHoriz :: Matrix Int -> Set (V2 Int)
visibleHoriz matrix = Set.unions $ do
    y <- [0 .. height - 1]
    let row = matrix Vec.! y
    pure $ Set.fromList $ do
        x <- [0 .. width - 1]
        let value = row Vec.! x
        let lefts = slice 0 x row
        let rights = slice (x + 1) width row
        guard $ Vec.all (< value) lefts || Vec.all (< value) rights
        pure $ V2 x y
  where
    height = Vec.length matrix
    width = maybe 0 Vec.length (matrix Vec.!? 0)

visibleVert :: Matrix Int -> Set (V2 Int)
visibleVert matrix = Set.unions $ do
    x <- [0 .. width - 1]
    let col = Vec.map (Vec.! x) matrix
    pure $ Set.fromList $ do
        y <- [0 .. height - 1]
        let value = col Vec.! y
        let ups = slice 0 y col
        let downs = slice (y + 1) height col
        guard $ Vec.all (< value) ups || Vec.all (< value) downs
        pure $ V2 x y
  where
    height = Vec.length matrix
    width = maybe 0 Vec.length (matrix Vec.!? 0)

visible :: Matrix Int -> Set (V2 Int)
visible matrix = visibleHoriz matrix `Set.union` visibleVert matrix

visibleFrom :: V2 Int -> Matrix Int -> V4 [V2 Int]
visibleFrom (V2 x y) matrix = V4 rights downs lefts ups
  where
    lefts = takeWhilst lesser [ V2 u y | u <- [x-1, x-2 .. 0] ]
    rights = takeWhilst lesser [ V2 u y | u <- [x+1 .. width-1] ]
    ups = takeWhilst lesser [ V2 x v | v <- [y-1, y-2 .. 0] ]
    downs = takeWhilst lesser [ V2 x v | v <- [y+1 .. height-1] ]
    value = matrix Vec.! y Vec.! x
    lesser (V2 u v) = matrix Vec.! v Vec.! u < value
    height = Vec.length matrix
    width = maybe 0 Vec.length (matrix Vec.!? 0)

{-# ANN scenicScore "HLint: ignore" #-}
scenicScore :: V2 Int -> Matrix Int -> Int
scenicScore pt matrix = product $ fmap length $ visibleFrom pt matrix

{-# ANN indices "HLint: ignore" #-}
indices :: Matrix Int -> Set (V2 Int)
indices matrix = Set.unions $ do
    x <- [0 .. width - 1]
    pure $ Set.fromList $ do
        y <- [0 .. height - 1]
        pure $ V2 x y
  where
    height = Vec.length matrix
    width = maybe 0 Vec.length (matrix Vec.!? 0)

part1 :: Matrix Int -> Int
part1 = visible >>> Set.size

{-# ANN part2 "HLint: ignore" #-}
part2 :: Matrix Int -> Int
part2 matrix = indices matrix
    & Set.map (\pt -> scenicScore pt matrix)
    & Set.foldl' max 0

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-08.txt"
    case Parse.runParser (parseMatrix <* Parse.eof) "day-08" text of
        Left err -> die $ Parse.errorBundlePretty err
        Right matrix -> do
            print $ part1 matrix
            print $ part2 matrix

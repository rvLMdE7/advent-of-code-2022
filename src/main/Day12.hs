{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Day12 where

import Control.Applicative (some)
import Control.Arrow ((<<<), (>>>))
import Control.Monad (when)
import Control.Monad.State (State)
import Control.Monad.State qualified as State
import Data.Char qualified as Char
import Data.Containers.ListUtils (nubInt)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Matrix (Matrix)
import Data.Matrix qualified as Matrix
import Data.PQueue.Prio.Min (MinPQueue)
import Data.PQueue.Prio.Min qualified as Queue
import GHC.Generics (Generic)
import Linear (V2(V2))
import Optics ((%), (&), zoom, use, at)
import Optics.State.Operators ((%=), (?=))
import System.Exit (die)
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.C

import Common (Parser, cons, readInputFileUtf8)


-- types

data Extended a = Base a | Infinity
    deriving (Eq, Ord, Show)

data Algo = MkAlgo
    { openSet :: MinPQueue (Extended Int) (V2 Int)
    , cameFrom :: Map (V2 Int) (V2 Int)
    , gScore :: Map (V2 Int) (Extended Int)
    , fScore :: Map (V2 Int) (Extended Int) }
    deriving (Eq, Generic, Ord, Show)


-- parsing

parseRow :: Parser [Char]
parseRow = some $ Parse.satisfy $ not <<< Char.isSpace

parseMatrix :: Parser (Matrix Char)
parseMatrix = do
    rows <- parseRow `Parse.sepEndBy` Parse.C.eol
    case nubInt (length <$> rows) of
        [_] -> pure $ Matrix.fromLists rows
        _   -> fail "rows unequal in length"

parseHill :: Parser (V2 Int, V2 Int, Matrix Char)
parseHill = do
    mat <- parseMatrix

    -- watch out: the Matrix api uses (y, x) co-ordinates which are 1-based
    let pts = (,) <$> [1 .. Matrix.nrows mat] <*> [1 .. Matrix.ncols mat]
    let findPt c = do
            i <- List.findIndex (\pt -> mat Matrix.! pt == c) pts
            pure $ pts !! i

    case (findPt 'S', findPt 'E') of
        (Nothing, _) -> fail "no start 'S' on hill"
        (_, Nothing) -> fail "no end 'E' on hill"
        (Just s, Just e) -> do
            let hill = mat & Matrix.setElem 'a' s & Matrix.setElem 'z' e
            let mkV2 = uncurry $ flip V2
            pure (mkV2 s, mkV2 e, hill)


-- general-purpose functions

add :: Num a => Extended a -> Extended a -> Extended a
add (Base x) (Base y) = Base (x + y)
add _        _        = Infinity

-- | Adds the given key-value pair into the queue, unless the value is already
-- present somewhere in the queue, in which case it does nothing.
addQueue :: (Ord k, Eq a) => k -> a -> MinPQueue k a -> MinPQueue k a
addQueue key val queue
    | val `elem` Queue.elemsU queue = queue
    | otherwise = Queue.insert key val queue

popQueue :: Ord k => State (MinPQueue k a) (Maybe a)
popQueue = State.state $ \q -> case Queue.minView q of
    Nothing        -> (Nothing, q)
    Just (val, q') -> (Just val, q')

adjacent :: Num a => V2 a -> [V2 a]
adjacent (V2 x y) =
    [ V2 x (y + 1)
    , V2 (x + 1) y
    , V2 x (y - 1)
    , V2 (x - 1) y ]

manhattan :: Num a => V2 a -> V2 a -> a
manhattan u v = sum $ abs $ v - u


-- A* implementation

aStar :: Matrix Char -> V2 Int -> State Algo (Maybe [V2 Int])
aStar matrix goal = zoom #openSet popQueue >>= \case
    Nothing                -> pure Nothing
    Just cur | cur == goal -> fmap (reverse >>> Just) (backtrace cur)
    Just current           -> do
        let check nbr = inBounds nbr && (value nbr <= value current + 1)
        let neighbours = filter check $ adjacent current
        for_ neighbours $ \nbr -> do
            tentative <- add (Base 1) <$> findG current
            score <- findG nbr
            when (tentative < score) $ do
                let heuristic = tentative `add` Base (manhattan nbr goal)
                #cameFrom % at nbr ?= current
                #gScore % at nbr ?= tentative
                #fScore % at nbr ?= heuristic
                #openSet %= addQueue heuristic nbr
        aStar matrix goal
  where
    findG pt = Map.findWithDefault Infinity pt <$> use #gScore
    value (V2 x y) = Char.ord $ matrix Matrix.! (y, x)
    inBounds (V2 x y) =
        (1 <= x && x <= Matrix.ncols matrix) &&
        (1 <= y && y <= Matrix.nrows matrix)

backtrace :: V2 Int -> State Algo [V2 Int]
backtrace current = use (#cameFrom % at current) >>= \case
    Nothing   -> pure [current]
    Just prev -> cons current <$> backtrace prev

shortestPath :: V2 Int -> V2 Int -> Matrix Char -> Maybe [V2 Int]
shortestPath start goal matrix =
    State.evalState (aStar matrix goal) $ MkAlgo
        { openSet = Queue.singleton heuristic start
        , cameFrom = Map.empty
        , gScore = Map.singleton start (Base 0)
        , fScore = Map.singleton start heuristic }
  where
    heuristic = Base $ manhattan start goal


-- main hook

part1 :: V2 Int -> V2 Int -> Matrix Char -> Maybe Int
part1 start end hill = shortestPath start end hill <&> \path -> length path - 1

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-12.txt"
    case Parse.runParser (parseHill <* Parse.eof) "day-12" text of
        Left err -> die $ Parse.errorBundlePretty err
        Right (start, end, hill) -> do
            part1 start end hill
                & maybe "no path exists" show
                & putStrLn

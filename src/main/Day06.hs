{-# LANGUAGE BangPatterns #-}

module Day06 where

import Data.ByteString (ByteString)
import Data.ByteString qualified as Bytes
import Data.Set qualified as Set
import Data.Function ((&))


distinct :: Ord a => [a] -> Bool
distinct = go Set.empty
  where
    go seen = \case
        []     -> True
        x : xs -> (x `Set.notMember` seen) && go (Set.insert x seen) xs

startOfMessageMarker :: ByteString -> Maybe Int
startOfMessageMarker = firstDistinctN 14

startOfPacketMarker :: ByteString -> Maybe Int
startOfPacketMarker = firstDistinctN 4

firstDistinctN :: Int -> ByteString -> Maybe Int
firstDistinctN n = go 0
  where
    go :: Int -> ByteString -> Maybe Int
    go !i bytes
        | Bytes.length bytes < n = Nothing
        | distinct $ Bytes.unpack $ Bytes.take n bytes = Just i
        | otherwise = go (i + 1) (Bytes.drop 1 bytes)

endOfMessageMarker :: ByteString -> Maybe Int
endOfMessageMarker bytes = (+ 14) <$> startOfMessageMarker bytes

endOfPacketMarker :: ByteString -> Maybe Int
endOfPacketMarker bytes = (+ 4) <$> startOfPacketMarker bytes

part1 :: ByteString -> Maybe Int
part1 = endOfPacketMarker

part2 :: ByteString -> Maybe Int
part2 = endOfMessageMarker

main :: IO ()
main = do
    bytes <- Bytes.readFile "input/day-06.txt"
    part1 bytes & maybe "error: no packet marker" show & putStrLn
    part2 bytes & maybe "error: no message marker" show & putStrLn

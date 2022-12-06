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

startOfPacketMarker :: ByteString -> Maybe Int
startOfPacketMarker = go 0
  where
    go :: Int -> ByteString -> Maybe Int
    go !i bytes
        | Bytes.length bytes < 4 = Nothing
        | distinct $ Bytes.unpack $ Bytes.take 4 bytes = Just i
        | otherwise = go (i + 1) (Bytes.drop 1 bytes)

endOfPacketMarker :: ByteString -> Maybe Int
endOfPacketMarker bytes = (+ 4) <$> startOfPacketMarker bytes

part1 :: ByteString -> Maybe Int
part1 = endOfPacketMarker

main :: IO ()
main = do
    bytes <- Bytes.readFile "input/day-06.txt"
    part1 bytes & maybe "error: no packet marker" show & putStrLn

module Day20 where

import Data.Sequence qualified as Seq
import Data.Sequence (Seq((:<|), (:|>)))
import Data.Maybe (fromJust)
import Data.Function ((&))
import Control.Arrow ((>>>))


moveAtByLoop :: Int -> Int -> Seq a -> Seq a
moveAtByLoop at by list = moveAtBy at (absolute - at) list
  where
    target = at + by
    absolute = target `mod` Seq.length list

moveAtBy :: Int -> Int -> Seq a -> Seq a
moveAtBy at by list = case by `compare` 0 of
    LT -> case Seq.splitAt (at + 1) list of
        (ls :|> l, right) -> Seq.insertAt (Seq.length ls + by) l ls <> right
        (Seq.Empty, right) -> right
    GT -> case Seq.splitAt at list of
        (left, r :<| rs) -> left <> Seq.insertAt by r rs
        (left, Seq.Empty) -> left
    EQ -> list

-- simplest thing: zip with [0..] at the start, and then use that index to
-- decide which element to process next!

mix :: Seq Int -> Seq Int
mix list = list
    & Seq.zip (Seq.fromFunction len id)
    & loop 0
    & fmap snd
  where
    loop n
        | n < len   = mixNum n >>> loop (n + 1)
        | otherwise = id
    mixNum n foo = moveAtByLoop index num foo
      where
        atN (i, _num) = i == n
        index = fromJust (Seq.findIndexL atN foo)
        (_, num) = foo `Seq.index` index
    len = Seq.length list

main :: IO ()
main = pure ()

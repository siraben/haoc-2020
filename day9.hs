{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Criterion.Main
import Data.Bifunctor
import Data.Foldable (toList)
import Data.List
import Data.Maybe
import qualified Data.Sequence as Q
import qualified Data.Vector as V

-- credits to https://github.com/mstksg/advent-of-code-2020/blob/master/src/AOC/Challenge/Day09.hs
slidingWindows :: Int -> [Int] -> [Q.Seq Int]
slidingWindows n = uncurry go . first Q.fromList . splitAt n
  where
    go ws@(_ Q.:<| qs) = \case
      x : xs -> ws : go (qs Q.:|> x) xs
      [] -> [ws]
    go _ = const []

part1 :: [Int] -> Int
part1 xs = fromJust $ do
  _ Q.:|> x <- find isBad (slidingWindows 26 xs)
  pure x
  where
    isBad :: Q.Seq Int -> Bool
    isBad (xs Q.:|> x) = null $ do
      y : ys <- tails (toList xs)
      z <- ys
      guard $ (y + z) == x
    isBad _ = True

part2 :: Int -> V.Vector Int -> Int
part2 target inp = maxMinSum
  where
    inp' = V.scanl (+) 0 inp
    l = V.slice a (b - a) inp
    maxMinSum = uncurry (+) (V.foldl' (\(a, b) n -> (min n a, max n b)) (maxBound, minBound) l)
    (a, b) = go 0 0
    go !i !j =
      case compare s target of
        EQ -> (i, j)
        LT -> go i (succ j)
        GT -> go (succ i) j
      where
        s = inp' V.! j - inp' V.! i

main = do
  let dayNumber = 9
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  let inp' = read <$> inp :: [Int]
  let vinp = V.fromList inp'
  let n = part1 inp'
  print n
  print (part2 n vinp)
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp',
          bench "part2" $ whnf (part2 n) vinp
        ]
    ]

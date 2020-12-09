{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main
import Data.List
import Data.Maybe
import qualified Data.Vector as V

part1 :: [Int] -> Int
part1 inp' = head (snd (fromJust (find (\(p, r) -> isNotValid p (head r)) (splitAt 25 <$> tails inp'))))
  where
    isNotValid p n = null [(x, y) | x <- p, y <- p, x + y == n]

part2 :: Int -> V.Vector Int -> Int
part2 target inp = maxMinSum
  where
    inp' = V.scanl (+) 0 inp
    l = V.slice a (b - a) inp
    maxMinSum = uncurry (+) (V.foldl' (\(a,b) n -> (min n a, max n b)) (maxBound,minBound) l)
    (a, b) = go 0 0
    go !i !j  = case compare s target of
                   EQ -> (i,j)
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

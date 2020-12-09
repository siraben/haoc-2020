{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Maybe
import Criterion.Main
import qualified Data.IntSet as IS
import qualified Data.Vector as V
-- subl a b list = take (b - a) (drop a list)

part1 :: [Int] -> Int
part1 inp' = head (snd (fromJust (find (\(p,r) -> isNotValid p (head r)) (splitAt 25 <$> tails inp'))))
  where
    isNotValid p n = null [(x,y) | x <- p, y <- p, x + y == n]
{-
  (\l -> minimum l + maximum l) (fromJust (find ((== target) . sum) (segs inp')))
= head (map (\l -> minimum l + maximum l) (filter ((== target) . sum) (segs inp')))
= head (map (\l -> minimum l + maximum l) (filter ((== target) . sum) (segs inp')))

= head (foldr (\x y -> if sum x == target then (minimum x + maximum x):y else y) [] (concat (map tails (inits inp'))))
-- seems stuck
-}
-- part2 :: Int -> [Int] -> Int
part2 target inp = minimum l + maximum l
  where
    inp' = V.scanl (+) 0 inp
    l = V.slice a (b - a) inp
    n = length inp'
    (a,b) = head [(i,j) | i <- [0..n-1], j <- [i..n - 1], inp' V.! j - inp' V.! i == target]

findFirst f = fromJust . find f

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

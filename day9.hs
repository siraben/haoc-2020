{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Maybe

part1 :: [Int] -> Int
part1 inp' = head (snd (fromJust (find (\(p,r) -> isNotValid p (head r)) (splitAt 25 <$> iterate (drop 1) inp'))))
  where
    isNotValid p n = null [(x,y) | x <- p, y <- p, x + y == n]

part2 :: Int -> [Int] -> Int
part2 target inp' = (\l -> minimum l + maximum l) (fromJust (find ((== target) . sum) (segs inp')))
  where
    segs = concatMap tails . inits

main = do
  let dayNumber = 9
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  let inp' = read <$> inp :: [Int]
  let n = part1 inp'
  print n
  print (part2 n inp')
  -- defaultMain
  --   [ bgroup
  --       dayString
  --       [ bench "part1" $ whnf part1 inp,
  --         bench "part2" $ whnf part2 inp
  --       ]
  --   ]

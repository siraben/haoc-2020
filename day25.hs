{-# LANGUAGE BangPatterns #-}

import Criterion.Main
import Data.Semigroup

-- Bounded by 20201227
newtype B = B {getB :: Int}

instance Semigroup B where
  B x <> B y = B ((x * y) `mod` 20201227)

instance Monoid B where
  mempty = B 1

crack :: Int -> Int -> Int
crack k l = go 1 7
  where
    go !n !y
      | y == l = n
      | otherwise = go (succ n) ((y * k) `mod` 20201227)

part1 :: Int -> Int
part1 = crack 7

part2 :: (Int, Int) -> Int
part2 (cls, b) = transform cls b
  where
    transform :: Int -> Int -> Int
    transform loopSize = getB . mtimesDefault loopSize . B

main = do
  let dayNumber = 25
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  [a, b] <- map read . lines <$> readFile dayFilename :: IO [Int]
  let inp = a
  let cls = part1 inp
  print (part1 inp)
  print (part2 (cls, b))
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ nf part1 inp,
          bench "part2" $ nf part2 (cls, b)
        ]
    ]

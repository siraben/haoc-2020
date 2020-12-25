{-# LANGUAGE BangPatterns #-}

import Criterion.Main
import Data.Foldable
import Data.Maybe

-- Useful functions
findFirst :: Foldable f => (a -> Bool) -> f a -> a
findFirst f = fromJust . find f

iter :: (a -> a) -> Int -> a -> a
iter f = go
  where
    go 0 x = x
    go n x = go (pred n) $! f x

step (!v, !s) = ((v * s) `mod` 20201227, s)

transform :: Int -> Int -> Int
transform loopSize subjectNum = fst (iter step loopSize (1, subjectNum))

crack k l = fst (findFirst ((== l) . snd) (zip [1 ..] (iterate ((`mod` 20201227) . (* k) :: Int -> Int) 7)))

part1 (a,b) = (crack 7 a, crack 7 b)
part2 ((a,b),(cls,dls)) = (transform cls b, transform dls a)

main = do
  let dayNumber = 25
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  [a, b] <- map read . lines <$> readFile dayFilename :: IO [Int]
  let inp = (a,b)
  let inp'@(cls, dls) = part1 inp
  print (part1 inp)
  print (part2 (inp,inp'))
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp,
          bench "part2" $ whnf part2 (inp,inp')
        ]
    ]

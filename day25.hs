{-# LANGUAGE BangPatterns #-}

-- import Criterion.Main
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

step (!v,!s) = ((v*s) `mod` 20201227,s)

-- transform :: Integer -> Integer -> Integer
transform loopSize subjectNum = fst (iter step loopSize (1,subjectNum))
-- transform loopSize subjectNum = (subjectNum ^ loopSize) `mod` 20201227

crack k l = fst (findFirst ((== l). snd) (zip [1..] (iterate ((`mod` 20201227) . (* k) :: Int -> Int) 7)))

main = do
  let dayNumber = 25
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  [a,b] <- map read . lines <$> readFile dayFilename :: IO [Int]
  -- let inp = (a,b)
  let cardLoopSize = crack 7 a
  let doorLoopSize = crack 7 b
  print (cardLoopSize, doorLoopSize)
  print (transform cardLoopSize b, transform doorLoopSize a)
  -- defaultMain
  --   [ bgroup
  --       dayString
  --       [ bench "part1" $ whnf part1 inp,
  --         bench "part2" $ whnf part2 inp
  --       ]
  --   ]

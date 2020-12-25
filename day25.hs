{-# LANGUAGE BangPatterns #-}
import Criterion.Main

iter :: (a -> a) -> Int -> a -> a
iter f = go
  where
    go 0 x = x
    go !n x = go (pred n) $! f x

transform :: Int -> Int -> Int
transform loopSize subjectNum = iter step loopSize 1
  where
    step v = (v * subjectNum) `mod` 20201227

crack :: Int -> Int -> Int
crack k l = go 1 7
  where
    go !n !y | y == l = n
             | otherwise = go (succ n) ((y * k) `mod` 20201227)

part1 :: (Int,Int) -> (Int,Int)
part1 (a, b) = (crack 7 a, crack 7 b)

part2 :: ((Int,Int),(Int,Int)) -> (Int,Int)
part2 ((a, b), (cls, dls)) = (transform cls b, transform dls a)

main = do
  let dayNumber = 25
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  [a, b] <- map read . lines <$> readFile dayFilename :: IO [Int]
  let inp = (a, b)
  let inp' = part1 inp
  print (part1 inp)
  print (part2 (inp, inp'))
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ nf part1 inp,
          bench "part2" $ nf part2 (inp, inp')
        ]
    ]

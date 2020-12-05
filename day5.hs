{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
import Criterion.Main
import qualified Data.ByteString.Char8 as B

solve :: B.ByteString -> Int
solve l = r * 8 + c
  where
    (a, b) = B.splitAt 7 l
    go s 'F' = loHalf s
    go s 'B' = upHalf s
    r = fst (B.foldl' go (0, 127 :: Int) a)
    c = fst (B.foldl' fish (0, 7) b)
    fish s 'L' = loHalf s
    fish s 'R' = upHalf s
    upHalf (l, h) = (l + ((1 + h - l) `div` 2), h)
    loHalf (l, h) = (l, l + ((h - l) `div` 2))

part1 :: [B.ByteString] -> Int
part1 = maximum . (solve <$>)

{-
Let a <= b, then
sum [0..a] = a(a+1) / 2
sum [0..b] = b(b+1) / 2
sum [a..b] = sum [0..b] - sum[0..a-1]
           = b(b+1) / 2 - (a-1)((a-1)+1) / 2
           = (b(b+1)-a(a-1))/2
[]

So, let a = 54; b = 930 in (b*(b+1)-a*(a-1)) `div` 2 == 431484
-}
part2 :: [B.ByteString] -> Int
part2 = (431484 -) . sum  . (solve <$>)

main = do
  let dayNumber = 5 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- B.lines <$> B.readFile dayFilename
  print (part1 inp)
  print (part2 inp)
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp,
          bench "part2" $ whnf part2 inp
        ]
    ]

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main (bench, bgroup, defaultMain, whnf)
import Data.Bits
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.List

splitOn' del bs = go bs
  where
    n = B.length del
    go bs = case B.breakSubstring del bs of
      (ls, rest) ->
        if B.null rest
          then ls : mempty
          else ls : splitOn' del (B.drop n rest)

part1 :: [[Int]] -> Int
part1 ls = sum [popCount $ foldl1' (.|.) l | l <- ls]

part2 :: [[Int]] -> Int
part2 ls = sum [popCount $ foldl1' (.&.) l | l <- ls]

main = do
  let dayNumber = 6 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- B.readFile dayFilename
  let fromStr' = foldl1' (.|.) . map (bit . (.&. 31) . ord)
  let inp' = map (fromStr' . B.unpack) . B.lines <$> splitOn' "\n\n" inp
  print (part1 inp')
  print (part2 inp')
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp',
          bench "part2" $ whnf part2 inp'
        ]
    ]

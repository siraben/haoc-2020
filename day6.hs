{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main
import qualified Data.ByteString.Char8 as B
import qualified Data.IntSet as IS
import Data.Char

splitOn' del bs = go bs
  where
    n = B.length del
    go bs = case B.breakSubstring del bs of
      (ls, rest) ->
        if B.null rest
          then ls : mempty
          else ls : splitOn' del (B.drop n rest)

part1 ls = sum [IS.size $ IS.unions l | l <- ls]

part2 ls = sum [IS.size $ foldr1 IS.intersection l | l <- ls]

main = do
  let dayNumber = 6 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- B.readFile dayFilename
  let inp' = map (IS.fromList . map ord . B.unpack) . B.lines <$> splitOn' "\n\n" inp
  print (part1 inp')
  print (part2 inp')
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp',
          bench "part2" $ whnf part2 inp'
        ]
    ]

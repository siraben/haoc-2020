{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

splitOn' del bs =
  case B.breakSubstring del bs of
    (ls, rest) ->
      if B.null rest
        then ls : mempty
        else ls : splitOn' del (B.tail rest)

part1, part2 :: [[B.ByteString]] -> Int
part1 = sum . (f . B.concat <$>)
  where
    f = length . B.group . B.sort
part2 l = sum (length . foldr1 L.intersect <$> ((B.unpack <$>) <$> l))

main = do
  let dayNumber = 6 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- B.readFile dayFilename
  let inp' = B.words . B.unwords . B.lines <$> splitOn' "\n\n" inp
  print (part1 inp')
  print (part2 inp')
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp',
          bench "part2" $ whnf part2 inp'
        ]
    ]

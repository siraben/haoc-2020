{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main
import qualified Data.ByteString.Char8 as B
import qualified Data.Set as S

splitOn' del bs = go bs
  where
    n = B.length del
    go bs = case B.breakSubstring del bs of
              (ls, rest) ->
                if B.null rest
                  then ls : mempty
                  else ls : splitOn' del (B.drop n rest)

part1 ls = sum [S.size $ S.unions l | l <- ls]
part2 ls = sum [S.size $ foldr1 S.intersection l | l <- ls]

main = do
  let dayNumber = 6 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- B.readFile dayFilename
  let inp' = (S.fromList . B.unpack <$>) . B.words . B.unwords . B.lines <$> splitOn' "\n\n" inp
  print (part1 inp')
  print (part2 inp')
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp',
          bench "part2" $ whnf part2 inp'
        ]
    ]

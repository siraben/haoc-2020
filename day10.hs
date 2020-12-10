{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main
import Data.Foldable
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List

part1 :: [Int] -> Int
part1 inp' = uncurry (*) (foldl' f (0,0) ds)
  where
    ds = zipWith (-) (tail inp') inp'
    f (!a,!b) 1 = (a+1,b)
    f (!a,!b) 3 = (a,b+1)

part2 :: [Int] -> Int
part2 = findElseZero 0 . pathsToGoal . IS.fromList

findElseZero :: Int -> IM.IntMap Int -> Int
findElseZero = IM.findWithDefault 0

pathsToGoal :: IS.IntSet -> IM.IntMap Int
pathsToGoal is = res
  where
    -- define an IntMap in terms of itself, yay lazy evaluation!
    res = (`IM.fromSet` is) $ \i ->
      if i == goal
        then 1
        else
          findElseZero (i + 1) res
            + findElseZero (i + 2) res
            + findElseZero (i + 3) res
    goal = IS.findMax is

main = do
  let dayNumber = 10
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- (read <$>) . lines <$> readFile dayFilename :: IO [Int]
  let final = maximum inp + 3
  let inp' = 0 : sort inp ++ [final]
  print (part1 inp')
  print (part2 inp')
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp',
          bench "part2" $ whnf part2 inp'
        ]
    ]

{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main
import Data.Foldable
import Data.Function
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

valid a b = (b - a) <= 3

countL [] = 0
-- bad exponential time solution
-- perhaps drop b
countL (a : b : c : rest) = re + dr
  where
    -- drop a
    re = countL (b : c : rest)
    -- drop b if a->c still valid
    dr = if valid a c then countL (a : c : rest) else 0
countL [a, b] = 1
countL [a] = 1

-- f is the function to "fix" over in a weird way
foo target f = fix (\x -> f (M.fromSet x keys M.!))
  where
    keys = S.fromList [0 .. target]

-- | Count the number of items in a container where the predicate is true.
countTrue :: Foldable f => (a -> Bool) -> f a -> Int
countTrue p = length . filter p . toList

part1 inp' = countTrue (3 ==) ds * countTrue (1 ==) ds
  where
    ds = zipWith (-) (tail inp') inp'

part2 = findElseZero 0 . pathsToGoal . IS.fromList

findElseZero = IM.findWithDefault 0

pathsToGoal :: IS.IntSet -> IM.IntMap Int
pathsToGoal is = res
  where
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

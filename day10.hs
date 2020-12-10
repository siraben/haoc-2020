{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -fdefer-typed-holes -fno-warn-unused-imports #-}

import qualified Data.ByteString.Char8 as B
import Data.Foldable
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T

-- | Repeat a function until you get the same result twice.
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f = go
  where
    go !x
      | x == y = x
      | otherwise = go y
      where
        y = f x

-- Start working down here
part1, part2 :: _ -> Int
part1 i = undefined
part2 i = undefined

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
countL (a : b : []) = 1
countL [a] = 1

-- f is the function to "fix" over in a weird way
foo target f = fix (\x -> f (M.fromSet x keys M.!))
  where
    keys = S.fromList [0 .. target]

-- | Count the number of items in a container where the predicate is true.
countTrue :: Foldable f => (a -> Bool) -> f a -> Int
countTrue p = length . filter p . toList

main = do
  let dayNumber = 10
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- (read <$>) . lines <$> readFile dayFilename :: IO [Int]
  let final = maximum inp + 3
  let inp' = 0:sort inp ++ [final]
  let ds = zipWith (-) (tail inp') inp'
  print (countTrue (3==) ds * countTrue (1==) ds)
  let part2_sol = foo final $ \arr start ->
        if start == final
          then 1
          else sum [arr (start + i) | i <- [1 .. 3], start + i `elem` inp']
  print (part2_sol 0)

-- defaultMain
--   [ bgroup
--       dayString
--       [ bench "part1" $ whnf part1 inp,
--         bench "part2" $ whnf part2 inp
--       ]
--   ]

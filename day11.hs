{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -fdefer-typed-holes -fno-warn-unused-imports #-}

import Criterion.Main
import qualified Data.ByteString.Char8 as B
import Data.Foldable
import Data.List
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Maybe

-- | Count the number of items in a container where the predicate is true.
countTrue :: Foldable f => (a -> Bool) -> f a -> Int
countTrue p = length . filter p . toList

-- | Repeat a function until you get the same result twice.
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f = go
  where
    go !x
      | x == y = x
      | otherwise = go y
      where
        y = f x

nextState s r c g
  | s == 'L' && notElem '#' ns = '#'
  | s == '#' && countTrue (== '#') ns >= 4 = 'L'
  | otherwise = s
  where
    cellRef r c g = g IM.!? r >>= (IM.!? c)
    neighbors r c g = catMaybes [cellRef (r + x) (c + y) g | x <- [-1 .. 1], y <- [-1 .. 1], (x, y) /= (0, 0)]
    ns = neighbors r c g

takeWhile1 p [] = []
takeWhile1 p (x : ys)
  | p x = x : takeWhile1 p ys
  | otherwise = [x]

nextState2 r c g
  | x == 'L' && notElem '#' ns = '#'
  | x == '#' && countTrue (== '#') ns >= 5 = 'L'
  | otherwise = x
  where
    x = (fromJust$ cellRef r c g)
    ray xo yo r c = catMaybes [cellRef x y g | (x, y) <- tail $ iterate (\(r,c) -> (xo + r, yo + c)) (r, c)]
    cellRef r c g = g IM.!? r >>= (IM.!? c)
    neighbors r c = concatMap (takeWhile1 (\x -> not (x == 'W' || x == 'L' || x == '#'))) [n, ne, e, se, s, sw, w, nw]
      where
        nw = ray (-1) 1 r c
        n = ray 0 1 r c
        ne = ray 1 1 r c
        e = ray 1 0 r c
        se = ray 1 (-1) r c
        s = ray 0 (-1) r c
        sw = ray (-1) (-1) r c
        w = ray (-1) 0 r c

    ns = neighbors r c

part1 = countTrue (== '#') . concat . (IM.elems <$>) . IM.elems . fixedPoint part1f

part1f ::  IntMap (IntMap Char)  ->  IntMap (IntMap Char)
part1f g = IM.mapWithKey (\r m -> IM.mapWithKey (\c s -> nextState s r c g ) m) g

part2 = countTrue (== '#') . unlines . fixedPoint part2f

part2f inp = [[nextState2 r c grid | c <- [1 .. nc]] | r <- [1 .. nr]]
  where
    (nr, nc) = (length inp, length (head inp))
    padLine l = 'W' : l ++ ['W']
    padded inp = padLine <$> (replicate nc 'W' : inp ++ [replicate nc 'W'])
    grid = IM.fromList (zip [0 ..] (IM.fromList . zip [0 ..] <$> padded inp))

main = do
  let dayNumber = 11
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  let grid = IM.fromList (zip [0 ..] (IM.fromList . zip [0 ..] <$> inp))
  print (part1 grid)
  print (part2 inp)
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 grid,
          bench "part2" $ whnf part2 inp
        ]
    ]

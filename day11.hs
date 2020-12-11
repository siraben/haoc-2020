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
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T


-- | Count the number of items in a container where the predicate is true.
countTrue :: Foldable f => (a -> Bool) -> f a -> Int
countTrue p = length . filter p . toList

-- | Build a frequency map
freqs :: (Foldable f, Ord a) => f a -> Map a Int
freqs = M.fromListWith (+) . map (,1) . toList

-- | map a function over elems satisfying a predicate
mapIf p f = foldl' (\xs x -> if p x then f x : xs else x : xs) []

-- | Set the element at index @n@ to @x@
setAt n x = (\(l, r) -> l ++ x : tail r) . splitAt n

-- | Like @findIndices@ but also return the element found for each index
findIndicesElem :: Foldable t => (a -> Bool) -> t a -> [(a, Int)]
findIndicesElem p = fst . foldl' go ([], 0 :: Int)
  where
    go (l, n) x
      | p x = ((x, n) : l, n + 1)
      | otherwise = (l, n + 1)

-- | Perturb a list's elements satisfying a predicate with a function
pertubationsBy :: (a -> Bool) -> (a -> a) -> [a] -> [[a]]
pertubationsBy p f l = [setAt n (f x) l | (x, n) <- findIndicesElem p l]

-- | Unconditional pertubation
pertubations :: (a -> a) -> [a] -> [[a]]
pertubations = pertubationsBy (const True)

-- | Generate all the segments of a list, O(n^2)
segs :: [a] -> [[a]]
segs = concatMap tails . inits

-- | Repeat a function until you get the same result twice.
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f = go
  where
    go !x
      | x == y = x
      | otherwise = go y
      where
        y = f x

nextState r c g
  | x == 'L' && notElem '#' ns = '#'
  | x == '#' && countTrue (== '#') ns >= 4 = 'L'
  | otherwise = x
  where
    cellRef r c g = do x <- (g M.!? r)
                       x M.!? c
    x = fromJust (cellRef r c g)
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
    cellRef r c g = (g M.! r) M.! c
    x = cellRef r c g
    ray xo yo r c = [cellRef x y g | (x, y) <- tail $ iterate (\(r,c) -> (xo + r, yo + c)) (r, c)]
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

part1 = countTrue (== '#') . concat . (M.elems <$>) . M.elems . fixedPoint part1f

part1f g = grid [[nextState r c g | c <- [0 .. nc - 1]] | r <- [0 .. nr - 1]]
  where
    (nr, nc) = (M.size g, M.size (g M.! 1))
    grid inp = M.fromList (zip [0 ..] (M.fromList . zip [0 ..] <$> inp))

part2 = countTrue (== '#') . unlines . fixedPoint part2f

part2f inp = [[nextState2 r c grid | c <- [1 .. nc]] | r <- [1 .. nr]]
  where
    (nr, nc) = (length inp, length (head inp))
    padLine l = 'W' : l ++ ['W']
    padded inp = padLine <$> (replicate nc 'W' : inp ++ [replicate nc 'W'])
    grid = M.fromList (zip [0 ..] (M.fromList . zip [0 ..] <$> padded inp))

main = do
  let dayNumber = 11
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  let grid = M.fromList (zip [0 ..] (M.fromList . zip [0 ..] <$> inp))
  print (part1 grid)
  print (part2 inp)
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 grid,
          bench "part2" $ whnf part2 inp
        ]
    ]

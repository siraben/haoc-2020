{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -fdefer-typed-holes -fno-warn-unused-imports #-}

import Criterion.Main
import Data.Bifunctor
import Data.Bits
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Foldable
import Data.Function
import qualified Data.Graph as G
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Monad
import Control.Applicative
import qualified Text.ParserCombinators.ReadP as P
import Debug.Trace

-- Slow splitOn for prototyping
splitOn :: String -> String -> [String]
splitOn sep s = T.unpack <$> T.splitOn (T.pack sep) (T.pack s)

-- ByteString splitOn
splitOn' :: B.ByteString -> B.ByteString -> [B.ByteString]
splitOn' del bs = go bs
  where
    n = B.length del
    go bs = case B.breakSubstring del bs of
      (ls, rest) ->
        if B.null rest
          then ls : mempty
          else ls : splitOn' del (B.drop n rest)

-- Useful functions
findFirst :: Foldable f => (a -> Bool) -> f a -> a
findFirst f = fromJust . find f

slidingWindows :: Int -> [Int] -> [[Int]]
slidingWindows n l = take n <$> tails l

-- | Compute the average of the elements of a container.
avg :: (Integral c, Foldable t) => t c -> c
avg = uncurry div . foldl' (\(s, l) x -> (x + s, succ l)) (0, 0)

swap (a, b) = (b, a)

dup a = (a, a)

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
        | x == y    = x
        | otherwise = go y
      where
        y = f x

-- Start working down here
part1, part2 :: _ -> Int
part1 i = undefined
part2 i = undefined

-- parseInput = 
data Range = Range Int Int deriving Show
cR :: Range -> Range -> (Int -> Bool)
cR (Range l1 h1) (Range l2 h2) n = within n l1 h1 || within n l2 h2
  where
    within n a b = a <= n && n <= b
ranges = [Range 25 80  `cR` Range 90 961
         ,Range 41 133 `cR` Range 148 968
         ,Range 48 425 `cR` Range 451 952
         ,Range 25 371 `cR` Range 384 966
         ,Range 49 531 `cR` Range 546 973
         ,Range 45 641 `cR` Range 656 954
         ,Range 43 357 `cR` Range 364 969
         ,Range 40 669 `cR` Range 689 954
         ,Range 40 550 `cR` Range 570 956
         ,Range 49 854 `cR` Range 863 953
         ,Range 48 601 `cR` Range 614 964
         ,Range 27 698 `cR` Range 715 962
         ,Range 38 781 `cR` Range 800 970
         ,Range 47 824 `cR` Range 842 965
         ,Range 45 219 `cR` Range 241 955
         ,Range 47 388 `cR` Range 401 954
         ,Range 42 906 `cR` Range 919 965
         ,Range 40 726 `cR` Range 733 955
         ,Range 27 161 `cR` Range 174 974
         ,Range 48 103 `cR` Range 110 954]

rangesA = zip [0..] ranges

myTicket :: [Int]
myTicket = [181,131,61,67,151,59,113,101,79,53,71,193,179,103,149,157,127,97,73,191]

parseLine :: P.ReadP [Int]
parseLine = P.sepBy (P.readS_to_P reads) (P.string ",") <* P.string "\n"
ranges2 = [Range 1 3  `cR` Range 5 7
          ,Range 6 11 `cR` Range 33 44
          ,Range 13 40 `cR` Range 45 50]

type Ticket = [Int]

-- for every permutation of ranges, filter the ones that fully satisfy
-- this ticket with zipped
p2 t = [l | l <- permutations rangesA, and (zipWith ($) (snd <$> l) t)]

-- step
p3 t ar  = [l | l <- ar, and (zipWith ($) (snd <$> l) t)]

-- extend ps t = unionBy ((==) `on` (fst <$>)) (p3 t =<< ps)
-- extend t ps = trace (show (length ps)) (p3 t =<< ps)

-- solve = foldl' p3 (permutations rangesA)

solve' ar = foldr p3 (permutations ar)

ranges3 = [Range 0 1  `cR` Range 4 19
          ,Range 0 5 `cR` Range 8 19
          ,Range 0 13 `cR` Range 16 19]

main = do
  let dayNumber = 16
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- readFile dayFilename
  let (fo,"") = last (P.readP_to_S (P.many parseLine) inp)
  -- let ranges = [Range 1 3  `cR` Range 5 7
  --              ,Range 6 11 `cR` Range 33 44
  --              ,Range 13 40 `cR` Range 45 50]
  -- ticket is valid if for all values it satisfies any of the range
  let isValidTicket l = and [any ($ f) ranges | f <- l]
  let bar = fo >>= (\l -> [f | f <- l, not (any ($ f) ranges)])
  -- print bar
  -- print (sum bar)
  let inp' = filter isValidTicket fo
  -- print (map fst <$> (p3 (permutations rangesA) (head inp')))
  
  print ((fst <$> head (solve' rangesA inp')))

  -- let inp' = last (P.readP_to_S parseInput inp)
  -- print (take 10 inp)
  print ()
  -- print (part1 inp)
  -- print (part2 inp)
  -- defaultMain
  --   [ bgroup
  --       dayString
  --       [ bench "part1" $ whnf part1 inp,
  --         bench "part2" $ whnf part2 inp
  --       ]
  --   ]

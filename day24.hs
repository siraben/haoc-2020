{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -fdefer-typed-holes -fno-warn-unused-imports #-}

import Control.Applicative
import Control.Monad
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
import Data.Ix
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
import qualified Text.ParserCombinators.Parsec as P

-- splitOn for strings
splitOn :: String -> String -> [String]
splitOn sep s = T.unpack <$> T.splitOn (T.pack sep) (T.pack s)

-- splitOn for ByteString
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
fixedPoint = fixedPointBy (==)

-- | Repeat a function until some condition is met.
fixedPointBy :: (a -> a -> Bool) -> (a -> a) -> a -> a
fixedPointBy cmp f = go
  where
    go !x
      | x `cmp` y = x
      | otherwise = go $! y
      where
        y = f $! x

data Dir = E | SE | SW | W | NW | NE
         deriving (Show, Read, Enum)

moves = enumFrom E
dir :: P.Parser Dir
dir = read <$> foldl' (\x y -> P.try y <|> x) mzero (P.string <$> ["E", "SE", "SW", "W", "NW", "NE"])

pp = P.parse (P.many1 dir) ""

pA (a,b,c) (x,y,z) = (a+x,b+y,c+z)
move E = pA (1,-1,0)
move SE = pA (0,-1,1)
move SW = pA (-1,0,1)
move W = pA (-1,1,0)
move NW = pA (0,1,-1)
move NE = pA (1,0,-1)

origin = (0,0,0)

follow = foldl' (flip move) origin
data Col = WH | BL deriving (Show, Eq)

fC WH = BL
fC BL = WH

type HG = Map (Int,Int,Int) Col
ba :: HG -> [Dir] -> HG
ba m p = M.insert coord (fC col) m
  where
    coord = follow p
    col = fromMaybe WH (m M.!? coord)

neighbors p = map (`move` p) moves

-- Start working down here
part1, part2 :: _ -> Int
part1 i = undefined
part2 i = undefined

step :: HG -> HG
step m = M.mapWithKey f (M.union m m')
  where
    m' = M.fromList ((,WH) <$> (neighbors =<< M.keys m))
    f c w = case w of
              WH -> if blacks == 2 then BL else WH
              BL -> if blacks == 0 || blacks > 2 then WH else BL
      where
        ns = neighbors c
        ncs = fromMaybe WH . (m M.!?) <$> ns
        blacks = countTrue (== BL) ncs
        whites = countTrue (== WH) ncs
iter 0 x = x
iter n x = iter (n-1) $! step x
    
main = do
  let dayNumber = 24
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- map (map toUpper) . lines <$> readFile dayFilename
  let Right inp' = traverse pp inp
  print (take 10 inp')
  let pat = (foldl' ba M.empty inp')
  let f = M.size . M.filter (== BL)
  print (f pat)
  print (f (iter 100 pat))
  -- print (part1 inp)
  -- print (part2 inp)
  -- defaultMain
  --   [ bgroup
  --       dayString
  --       [ bench "part1" $ whnf part1 inp,
  --         bench "part2" $ whnf part2 inp
  --       ]
  --   ]

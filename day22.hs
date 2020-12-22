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
import qualified Text.ParserCombinators.ReadP as P

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
      | x == y = x
      | otherwise = go $! y
      where
        y = f $! x

-- Start working down here
type GS = ([Int], [Int])

step :: GS -> GS
step (a : as, b : bs)
  | a > b = (as ++ l, bs)
  | a < b = (as, bs ++ l)
  where
    l = [max a b, min a b]
step x = x

calcScore l = sum (zipWith (*) [1 ..] (reverse l))

calcWin ([], l) = calcScore l
calcWin (l, []) = calcScore l

part1 (a, b) = calcWin (fixedPoint step (a, b))

type GH = [GS]
{-

(1) Before either player deals a card, if there was a previous round
    in this game that had exactly the same cards in the same order in
    the same players' decks, the game instantly ends in a win for
    player 1. Previous rounds from other games are not
    considered. (This prevents infinite games of Recursive Combat,
    which everyone agrees is a bad idea.)

(2) Otherwise, this round's cards must be in a new configuration; the
    players begin the round by each drawing the top card of their deck
    as normal.

(3) If both players have at least as many cards remaining in their
    deck as the value of the card they just drew, the winner of the
    round is determined by playing a new game of Recursive Combat (see
    below).

(4) Otherwise, at least one player must not have enough cards left in
    their deck to recurse; the winner of the round is the player with
    the higher-value card.

-}
--     in prog, one, two
data S = P | O | T deriving (Show, Eq)

step2 :: (GS,GH,S) -> (GS,GH,S)
step2 (g@([], b),h,_) = (g,h,T)
step2 (g@(a,[]),h,_) = (g,h,O)
step2 (g@(a:as,b:bs),h,s) | g `elem` h = (g,h,O) -- (1)
                          | a <= length as && b <= length bs =
                            let l = case s of
                                  O -> [a,b]
                                  T -> [b,a]
                                subgameRes@(g',_,s) = fixedPoint step2 ((take a as,take b bs),[],P)
                            in case s of -- (3)
                                 T -> ((as, bs ++ l),g:h, P)
                                 O -> ((as ++ l, bs),g:h, P)
                                 
                          | otherwise = (step g, g:h, s)
                          
ex2 = (([9, 2, 6, 3, 1],[5, 8, 4, 7, 10]), [],P)
main = do
  let dayNumber = 22
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- (tail . lines <$>) . splitOn "\n\n" <$> readFile dayFilename
  let [a, b] = map (map read) inp :: [[Int]]
  print (part1 (a, b))
  print (calcWin (let (x,_,_) = fixedPoint step2 ((a,b), [], P) in x))

-- print (part1 inp)
-- print (part2 inp)
-- defaultMain
--   [ bgroup
--       dayString
--       [ bench "part1" $ whnf part1 inp,
--         bench "part2" $ whnf part2 inp
--       ]
--   ]

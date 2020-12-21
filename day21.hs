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

-- import Control.Applicative
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
import Text.ParserCombinators.Parsec

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
      | otherwise = go y
      where
        y = f x

--       ing list   allergens
type L = ([String], [String])

tok :: Parser a -> Parser a
tok p = p <* spaces

symb = tok . string
word = many1 letter

parseL :: Parser L
parseL = do l <- many1 (tok word)
            string "(contains "
            r <- sepBy (tok word) (symb ",")
            pure (l,r)

pp = parse parseL ""
-- Start working down here
-- part1, part2 :: _ -> Int
-- part1 i = undefined
-- part2 i = undefined
cp l r = [(a,b) | a <- l, b <- r]

-- Possible assignments
type SolSet = [Map String String]
aa ls = (name, S.fromList (snd <$> ls))
  where
    name = fst (head ls)

ldist a l = (a,) <$> l
addC :: (Ord a, Ord k) => Map k (Set a) -> (k, a) -> Map k (Set a)
addC m (a,b) = M.adjust (S.insert b) a m'
  where
    m' = if M.notMember a m then M.insert a S.empty m else m

dd a i = [(x,) <$> i | x <- a]

-- backsol :: (Int, ([[Int]], IntSet)) -> Maybe (Int, ([[Int]], IntSet))
-- (,) . fst <$> S.minView (sols S.\\ solset) <*> pure (l', sols)
-- backsol (n, (l, solset)) = (l', sols)
--   where
--     sols = S.fromList . concat . filter ((== 1) . length) $ l'
--     l' = map f l
--     f l = if length l == 1 then l else filter (/= n) l

-- backsol :: (String, [(String, Set String)]) -> (String, ([(String, Set String)], Set ))

report = concat . intersperse ","
sol :: [(String, String)]
sol = [("shellfish", "clg"), ("peanuts", "lxjtns"), ("sesame", "vzzz"), ("soy", "cxfz"), ("eggs", "prxmdlz"), ("wheat", "qdfpq"), ("nuts", "knprxg"), ("fish", "ncjv")]

main = do
  let dayNumber = 21
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  let Right inp'' = traverse pp inp
  let inp' = swap <$> inp''
  let allIngreds' = freqs (concatMap snd inp')
  let allIngreds = S.fromList (concatMap snd inp')
  let bb = uncurry cp <$> inp'
  let constraints = M.unionsWith S.intersection (uncurry M.singleton <$> (map aa . uncurry dd =<< inp')) :: Map String (Set String)
  let badIngreds = S.unions (M.elems constraints)
  let cList = M.toList constraints
  let dd = cList :: [(String, Set String)]
  let e = [(x, S.elems s) | (x,s) <- dd]
  mapM_ print cList
  let goodIngreds = S.elems (allIngreds S.\\ badIngreds)
  print (sum [allIngreds' M.! i |i <-  goodIngreds])
  -- print (part1 inp)
  -- print (part2 inp)
  -- defaultMain
  --   [ bgroup
  --       dayString
  --       [ bench "part1" $ whnf part1 inp,
  --         bench "part2" $ whnf part2 inp
  --       ]
  --   ]

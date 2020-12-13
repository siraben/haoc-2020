{-# LANGUAGE UnicodeSyntax, BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
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

-- |Extended Euclidean algorithm
-- Given A,B, finds integers X,Y that satisfy Bézout's identity:
--   A*X + B*Y = gcd(A,B)
egcd a b = aux a b 1 0 0 1
  where aux r 0  x y _  _  = (r,x,y)
        aux r r' x y x' y' = aux r' r'' x' y' x'' y''
          where r'' = r `rem` r'
                q   = r `div` r'
                x'' = x - q * x'
                y'' = y - q * y'

-- |Finds the modular multiplicative inverse of @a@ modulo @m@.
-- Returns number X (iff it exists) that satisfy:
--   X*A ≡ 1 (mod m).
invert a m = case egcd a m of
              (1,x,_) -> Just x
              _       -> Nothing 

-- Syntax sugar.

-- |Solves a system of simultaneous congruences.
-- Chinese Remainder theorem determines a number X:
--   X ≡ a₀ (mod n₀)
--   X ≡ a₁ (mod n₁)
--   X ≡ a₂ (mod n₂)
--   ...
-- The input is a list of tuples: [(a₀,n₀), (a₁,n₁), (a₂,n₂)].
--
-- Note: On empty input, returns X ≡ 0 (mod 1).
class ChineseRemainder r where
  chineseRemainder ∷ [(Integer,Integer)] → r

-- Result is a tuple (X,M): X ≡ M
instance ChineseRemainder (Maybe (Integer,Integer)) where
  chineseRemainder = foldM aux (0,1)
    where aux (a,p) (b,q)
              | (a-b) `rem` k == 0 = Just (x, kmn)
              | otherwise          = Nothing
              where k       = gcd p q
                    m       = p `div` k
                    n       = q `div` k
                    (_,_,β) = k `egcd` (m*n)
                    (_,_,δ) = m `egcd` q
                    (_,_,ζ) = n `egcd` p
                    kmn     = p*n
                    x       = (a*β*m*n+a*δ*k*n+b*ζ*k*m) `rem` kmn

-- Result is a number X: X ≡ M (where M is a gcd of given modulis).
instance ChineseRemainder (Maybe Integer) where
  chineseRemainder = fmap fst . (chineseRemainder ∷ [(Integer,Integer)] -> Maybe (Integer,Integer))

-- Result is a tuple of infinite lists of integers Xs, Ys:
--   ∀x∈Xs: x≥0, x ≡ X (mod M)
--   ∀x∈Xs: y<0, y ≡ X (mod M)
-- where X is a solution to a system of given simultaneous congruences.
instance ChineseRemainder (Maybe ([Integer], [Integer])) where
  chineseRemainder = fmap aux . chineseRemainder
    where aux (x,m) | x<0       = ([x+m,x+2*m..], [x,x-m..])
                    | otherwise = ([x,x+m..],     [x-m,x-2*m..])

-- Result is an infinite list of integers Xs:
--   ∀x∈Xs: x ≡ X (mod M)
-- where X is a solution to a system of given simultaneous congruences.
instance ChineseRemainder (Maybe [Integer]) where
  chineseRemainder = fmap intertwine . chineseRemainder
    where intertwine ([],_) = []
          intertwine (_,[]) = []
          intertwine ((x:xs),(y:ys)) = x:y:intertwine (xs,ys)

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

bar l = filter ((/= 0) . snd) (zipWith (\x n -> ((x - n) `mod` x, x)) l (head l:[1..]))

main = do
  let dayNumber = 13
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  let target = 1002578
  let targetS = 939
  let l' = [7,13,59,31,19] :: [Integer]
  let l = [19,37,751,29,13,23,431,41,17] :: [Integer]
  let x = 0
  let inp' = [19,x,x,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,751,x,29,x,x,x,x,x,x,x,x,x,x,13,x,x,x,x,x,x,x,x,x,23,x,x,x,x,x,x,x,431,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,17] :: [Integer]
  print (bar inp')
  let foo n = (\x -> ((x * ((x + n) `div` x)) - n))
  let v = length l'
  print (foo target <$> l)
  -- print (foo 3417 <$> [17,13,19])
  -- print (foo 754018 <$> [67,7,59,61])
  -- print (foo 933913 <$> [67,7,59,61])
  print ((779210 `mod`) <$> [67,7,59,61]) -- 67,x,7,59,61
  print ((754018 `mod`) <$> [67,7,59,61])
  print ((933913 `mod`) <$> [67,7,59,61])
  
  -- print [x | x <- [1..], let a = foo x <$> l', a == [2..v]]
  -- print [foo x <$> l' | x <- [1..]]
  print (head . filter (> 0) <$> chineseRemainder (bar inp') :: Maybe Integer)
  print ()
  -- print (take 10 inp)
  -- print (part1 inp)
  -- print (part2 inp)
  -- defaultMain
  --   [ bgroup
  --       dayString
  --       [ bench "part1" $ whnf part1 inp,
  --         bench "part2" $ whnf part2 inp
  --       ]
  --   ]

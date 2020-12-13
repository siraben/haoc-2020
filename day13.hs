{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

import Control.Applicative
import Control.Monad
import Criterion.Main
import Data.Foldable
import Data.Function
import Data.Functor
import qualified Text.ParserCombinators.ReadP as P

-- credits to https://gist.github.com/kgadek/5503271
-- | Extended Euclidean algorithm
--  Given A,B, finds integers X,Y that satisfy Bézout's identity:
--    A*X + B*Y = gcd(A,B)
egcd a b = aux a b 1 0 0 1
  where
    aux r 0 x y _ _ = (r, x, y)
    aux r r' x y x' y' = aux r' r'' x' y' x'' y''
      where
        r'' = r `rem` r'
        q = r `div` r'
        x'' = x - q * x'
        y'' = y - q * y'


-- | Solves a system of simultaneous congruences.
--  Chinese Remainder theorem determines a number X:
--    X ≡ a₀ (mod n₀)
--    X ≡ a₁ (mod n₁)
--    X ≡ a₂ (mod n₂)
--    ...
--  The input is a list of tuples: [(a₀,n₀), (a₁,n₁), (a₂,n₂)].
--
--  Note: On empty input, returns X ≡ 0 (mod 1).
class ChineseRemainder r where
  chineseRemainder :: [(Integer, Integer)] -> r

-- Result is a tuple (X,M): X ≡ M
instance ChineseRemainder (Maybe (Integer, Integer)) where
  chineseRemainder = foldM aux (0, 1)
    where
      aux (a, p) (b, q)
        | (a - b) `rem` k == 0 = Just (x, kmn)
        | otherwise = Nothing
        where
          k = gcd p q
          m = p `div` k
          n = q `div` k
          (_, _, β) = k `egcd` (m * n)
          (_, _, δ) = m `egcd` q
          (_, _, ζ) = n `egcd` p
          kmn = p * n
          x = (a * β * m * n + a * δ * k * n + b * ζ * k * m) `rem` kmn

-- Result is a number X: X ≡ M (where M is a gcd of given modulis).
instance ChineseRemainder (Maybe Integer) where
  chineseRemainder = fmap fst . (chineseRemainder :: [(Integer, Integer)] -> Maybe (Integer, Integer))

-- Result is a tuple of infinite lists of integers Xs, Ys:
--   ∀x∈Xs: x≥0, x ≡ X (mod M)
--   ∀x∈Xs: y<0, y ≡ X (mod M)
-- where X is a solution to a system of given simultaneous congruences.
instance ChineseRemainder (Maybe ([Integer], [Integer])) where
  chineseRemainder = fmap aux . chineseRemainder
    where
      aux (x, m)
        | x < 0 = ([x + m, x + 2 * m ..], [x, x - m ..])
        | otherwise = ([x, x + m ..], [x - m, x -2 * m ..])

-- Result is an infinite list of integers Xs:
--   ∀x∈Xs: x ≡ X (mod M)
-- where X is a solution to a system of given simultaneous congruences.
instance ChineseRemainder (Maybe [Integer]) where
  chineseRemainder = fmap intertwine . chineseRemainder
    where
      intertwine ([], _) = []
      intertwine (_, []) = []
      intertwine (x : xs, y : ys) = x : y : intertwine (xs, ys)

bar l = filter ((/= 0) . snd) (zipWith (\x n -> ((x - n) `mod` x, x)) l (head l : [1 ..]))

part1 :: Integer -> [Integer] -> Integer
part1 target inp' = uncurry (*) (minimumBy (compare `on` snd) (foo target <$> filter (/= 0) inp'))
  where
    foo n = \x -> (x, (x * ((x + n) `div` x)) - n)

part2 :: [Integer] -> Maybe Integer
part2 inp' = head . filter (> 0) <$> chineseRemainder (bar inp') :: Maybe Integer

main = do
  let dayNumber = 13
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  (t : l) <- lines <$> readFile dayFilename
  let target = read t :: Integer
  let entry = P.readS_to_P reads <|> (P.string "x" $> 0)
  let inp' = case P.readP_to_S (P.sepBy entry (P.string ",") <* P.eof) (head l) of ((r, _) : _) -> r
  print (part1 target inp')
  print (part2 inp')
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf (part1 target) inp',
          bench "part2" $ whnf part2 inp'
        ]
    ]

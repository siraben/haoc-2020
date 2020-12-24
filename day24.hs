{-# LANGUAGE TupleSections #-}

import Control.Monad
import Criterion.Main
import Data.Char
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Text.ParserCombinators.Parsec

-- | Count the number of items in a container where the predicate is true.
countTrue :: Foldable f => (a -> Bool) -> f a -> Int
countTrue p = length . filter p . toList

data Dir = E | SE | SW | W | NW | NE
  deriving (Show, Read, Enum)

moves = enumFrom E

dir :: Parser Dir
dir = read <$> foldl' (\x y -> try y <|> x) mzero (string <$> ["E", "SE", "SW", "W", "NW", "NE"])

pp = parse (many1 dir) ""

pA (a, b, c) (x, y, z) = (a + x, b + y, c + z)

move E = pA (1, -1, 0)
move SE = pA (0, -1, 1)
move SW = pA (-1, 0, 1)
move W = pA (-1, 1, 0)
move NW = pA (0, 1, -1)
move NE = pA (1, 0, -1)

origin = (0, 0, 0)

follow = foldl' (flip move) origin

fC = not

type HG = Map (Int, Int, Int) Bool

neighbors p = map (`move` p) moves

step :: HG -> HG
step m = M.mapWithKey f (M.union m m')
  where
    m' = M.fromList ((,False) <$> (neighbors =<< M.keys m))
    f c w
      | w = not (blacks == 0 || blacks > 2)
      | blacks == 2 = True
      | otherwise = False
      where
        ns = neighbors c
        ncs = fromMaybe False . (m M.!?) <$> ns
        blacks = countTrue id ncs

countBlacks = M.size . M.filter id

ba :: HG -> [Dir] -> HG
ba m p = M.insert coord (fC col) m
  where
    coord = follow p
    col = Just True == (m M.!? coord)

createMap :: [[Dir]] -> HG
createMap = foldl' ba M.empty

part1 :: [[Dir]] -> Int
part1 = countBlacks . createMap

part2 :: [[Dir]] -> Int
part2 = countBlacks . iter 100 . createMap
  where
    iter 0 x = x
    iter n x = iter (n -1) $! step x

main = do
  let dayNumber = 24
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- map (map toUpper) . lines <$> readFile dayFilename
  let Right inp' = traverse pp inp
  print (part1 inp')
  print (part2 inp')
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp',
          bench "part2" $ whnf part2 inp'
        ]
    ]

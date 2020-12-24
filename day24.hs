{-# LANGUAGE TupleSections #-}

import Control.Monad
import Criterion.Main
import Data.Char
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Text.ParserCombinators.Parsec

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

follow = foldl' (flip move) origin
  where
    origin = (0, 0, 0)

neighbors p = S.fromList (map (`move` p) moves)

type Coord = (Int, Int, Int)

type PC = Set Coord

step2 :: PC -> PC
step2 ps = keep <> birth
  where
    ncs :: Map Coord Int
    ncs = M.unionsWith (+) (fmap (M.fromSet (const 1) . neighbors) (S.toList ps))
    keep = M.keysSet (M.filter (\n -> n == 1 || n == 2) (ncs `M.restrictKeys` ps))
    birth = M.keysSet (M.filter (== 2) (ncs `M.withoutKeys` ps))

type HG = Map (Int, Int, Int) Int

createMap :: [[Dir]] -> HG
createMap = foldl' addPoint M.empty

addPoint :: HG -> [Dir] -> HG
addPoint m p = M.insert coord (1 - col) m
  where
    coord = follow p
    col = if Just 1 == (m M.!? coord) then 1 else 0

part1 :: [[Dir]] -> Int
part1 = M.size . M.filter (== 1) . createMap

part2 :: [[Dir]] -> Int
part2 = S.size . iter 100 . toSet . createMap
  where
    toSet :: HG -> PC
    toSet = M.keysSet . M.filter (== 1)
    iter 0 x = x
    iter n x = iter (pred n) $! step2 x

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

{-# LANGUAGE TupleSections #-}

import Control.Monad
import Criterion.Main
import Data.Char
import Data.List
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

pA (a, b) (x, y) = (a + x, b + y)

move E = pA (1, -1)
move SE = pA (0, -1)
move SW = pA (-1, 0)
move W = pA (-1, 1)
move NW = pA (0, 1)
move NE = pA (1, 0)

follow = foldl' (flip move) origin
  where
    origin = (0, 0)

neighbors p = S.fromList (map (`move` p) moves)

type Coord = (Int, Int)

type PC = Set Coord

step2 :: PC -> PC
step2 ps = keep <> birth
  where
    ncs :: Map Coord Int
    ncs = M.unionsWith (+) (fmap (M.fromSet (const 1) . neighbors) (S.toList ps))
    keep = M.keysSet (M.filter (\n -> n == 1 || n == 2) (ncs `M.restrictKeys` ps))
    birth = M.keysSet (M.filter (== 2) (ncs `M.withoutKeys` ps))

type HG = Map Coord Bool

createMap :: [[Dir]] -> HG
createMap = foldl' addPoint M.empty

addPoint :: HG -> [Dir] -> HG
addPoint m p = M.insertWith f coord True m
  where
    coord = follow p
    f _ y = not y

part1 :: [[Dir]] -> Int
part1 = M.size . M.filter id . createMap

part2 :: [[Dir]] -> Int
part2 = S.size . iter 100 . toSet . createMap
  where
    toSet :: HG -> PC
    toSet = M.keysSet . M.filter id
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

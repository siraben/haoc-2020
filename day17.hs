{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Criterion.Main
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

iter :: (a -> a) -> Int -> a -> a
iter f = go
  where
    go 0 x = x
    go !n x = go (pred n) $! f x

type Coord = (Int, Int, Int)

type Coord4 = (Int, Int, Int, Int)

neighbors (a, b, c) = S.fromList [(a + x, b + y, c + z) | x <- l, y <- l, z <- l, (x, y, z) /= (0, 0, 0)]
  where
    l = [-1 .. 1]

neighbors4 (a, b, c, d) = S.fromList [(a + x, b + y, c + z, d + w) | x <- l, y <- l, z <- l, w <- l, (x, y, z, w) /= (0, 0, 0, 0)]
  where
    l = [-1 .. 1]

type PC = Set Coord

type PC4 = Set Coord4

step :: forall k. Ord k => (k -> Set k) -> (Int -> Bool) -> (Int -> Bool) -> Set k -> Set k
step ns aliveCond birthCond ps = keep <> birth
  where
    ncs :: Map k Int
    ncs =
      M.unionsWith (+) $
        S.toList ps <&> \p ->
          M.fromSet (const 1) (ns p)
    keep = M.keysSet (M.filter aliveCond (ncs `M.restrictKeys` ps))
    birth = M.keysSet (M.filter birthCond (ncs `M.withoutKeys` ps))

part1 :: [[Bool]] -> Int
part1 inp = S.size (iter (step neighbors (\n -> n == 2 || n == 3) (== 3)) 6 inp'')
  where
    inp' = concat (zipWith (\y l -> concatMap (\(x, c) -> [(x, y, 0) | c]) l) [0 ..] (zip [0 ..] <$> inp))
    inp'' :: PC
    inp'' = S.fromList inp'

part2 :: [[Bool]] -> Int
part2 inp = S.size (iter (step neighbors4 (\n -> n == 2 || n == 3) (== 3)) 6 inp'')
  where
    inp' = concat (zipWith (\y l -> concatMap (\(x, c) -> [(x, y, 0, 0) | c]) l) [0 ..] (zip [0 ..] <$> inp))
    inp'' :: PC4
    inp'' = S.fromList inp'

main = do
  let dayNumber = 17
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inpf <- lines <$> readFile dayFilename
  let conv c = case c of
        '#' -> True
        '.' -> False
        _ -> error $ "invalid cell: " ++ show c
  let inp = map (map conv) inpf
  print (part1 inp)
  print (part2 inp)
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp,
          bench "part2" $ whnf part2 inp
        ]
    ]

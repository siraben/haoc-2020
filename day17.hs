{-# LANGUAGE BangPatterns #-}

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

neighbors (a, b, c) = S.fromList [(a + x, b + y, c + z) | x <- l, y <- l, z <- l, (x, y, z) /= (0, 0, 0)]
  where
    l = [-1 .. 1]

type PC = Set Coord

step2 :: PC -> PC
step2 ps = keep <> birth
  where
    ncs :: Map Coord Int
    ncs =
      M.unionsWith (+) $
        S.toList ps <&> \p ->
          M.fromSet (const 1) (neighbors p)
    keep = M.keysSet (M.filter (\n -> n == 2 || n == 3) (ncs `M.restrictKeys` ps))
    birth = M.keysSet (M.filter (== 3) (ncs `M.withoutKeys` ps))

part1 inp = S.size (iter step2 6 inp'')
  where
    inp' = concat (zipWith (\y l -> concatMap (\(x,c) -> [(x,y,0) | c]) l) [0..] (zip [0..] <$> inp))
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
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp
        ]
    ]

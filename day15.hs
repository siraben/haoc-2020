{-# LANGUAGE BangPatterns #-}

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Criterion.Main
import Data.List

--        seen       time   last
data S = S { seen :: !(IntMap Int), time :: !Int, prev :: !Int }

initState (x:xs) = foldl' f (S mempty 0 x) xs
  where
    f (S m t prev) x = S (IM.insert prev t m) (t+1) x

step :: S -> S
step (S m t prev) = S (IM.insert prev t m) (t+1) res
  where
    res = case m IM.!? prev of
      Nothing -> 0
      Just n -> t - n

iter 0 x = x
iter n x = iter (n-1) $! step x

part1 inp = prev (iter (2020 - length inp) (initState inp))
part2 inp = prev (iter (30000000 - length inp) (initState inp))

main = do
  let inp = [0, 13, 1, 16, 6, 17]
  print (part1 inp)
  print (part2 inp)
-- defaultMain
--   [ bgroup
--       dayString
--       [ bench "part1" $ whnf part1 inp,
--         bench "part2" $ whnf part2 inp
--       ]
--   ]

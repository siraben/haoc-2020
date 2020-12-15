{-# LANGUAGE BangPatterns #-}

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Maybe
import Criterion.Main

iterN :: (a -> a) -> Int -> a -> a
iterN f = go
  where
    go 0 x = x
    go !n x = go (n - 1) (f x)

data Dat = One Int | Two Int Int deriving (Show)

type S = (IntMap Dat, Int)

push n (One i) = Two n i
push n (Two a _) = Two n a

pushM n Nothing = One n
pushM n (Just x) = push n x

initState l = (IM.fromList (zip l (One <$> [1 .. n])), length l)
  where
    n = length l

naive2 :: S -> Int -> (S, Int)
naive2 (m, t) n =
  if maybe 0 len hist < 2
    then ((IM.insert 0 ent m, t + 1), 0)
    else ((IM.insert res (pushM (t + 1) (m IM.!? res)) m, t + 1), res)
  where
    ent =
      case m IM.!? 0 of
        Nothing -> One (t + 1)
        Just n -> push (t + 1) n
    len (One _) = 1
    len (Two _ _) = 2
    hist = IM.lookup n m
    ext (Two a b) = a - b
    hist' = fromJust hist
    res = ext hist'

part1 inp = snd (iterN (uncurry naive2) (2020 - length inp) (initState inp, 0))

part2 inp = snd (iterN (uncurry naive2) (30000000 - length inp) (initState inp, 0))

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

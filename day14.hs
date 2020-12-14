{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Criterion.Main
import Data.Bits
import Data.Foldable
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Text.ParserCombinators.ReadP as P

data L = Mask String | Ass Int Int deriving (Show)

parseLine = mem <|> mask
  where
    mask = P.string "mask = " *> (Mask <$> P.manyTill (P.char 'X' <|> P.char '1' <|> P.char '0') (P.char '\n'))
    mem = do
      P.string "mem["
      l <- P.readS_to_P reads
      P.string "] = "
      r <- P.readS_to_P reads
      P.char '\n'
      pure (Ass l r)

type S = (String, IntMap Int)

ev :: S -> L -> S
ev (m, arr) (Mask s) = (s, arr)
ev (m, arr) (Ass l r) = (m, IM.insert l val arr)
  where
    val = ref m r

ref :: String -> Int -> Int
ref l = foldl' g id l'
  where
    l' = zip [0 ..] (reverse l)
    g f (n, 'X') = f
    g f (n, '0') = \x -> f (clearBit x n)
    g f (n, '1') = \x -> f (setBit x n)

ref2 :: String -> Int -> [Int]
ref2 l = foldl' g pure l'
  where
    l' = zip [0 ..] (reverse l)
    g f (n, 'X') = \x -> f =<< [setBit x n, clearBit x n]
    g f (n, '0') = f
    g f (n, '1') = \x -> f (setBit x n)

foldInsert :: IntMap a -> [(Int, a)] -> IntMap a
foldInsert m [] = m
foldInsert m ((l, r) : b) = foldInsert (IM.insert l r m) b

ev2 :: S -> L -> S
ev2 (m, arr) (Mask s) = (s, arr)
ev2 (m, arr) (Ass l r) = (m, foldInsert arr vals)
  where
    vals = [(loc, r) | loc <- ref2 m l]

part1 inp' = sum (IM.elems (snd (foldl' ev (mempty, mempty) inp')))

part2 inp' = sum (IM.elems (snd (foldl' ev2 (mempty, mempty) inp')))

main = do
  let dayNumber = 14 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- readFile dayFilename
  let (inp', _) = last (P.readP_to_S (P.many parseLine) inp)
  print (part1 inp')
  print (part2 inp')
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp',
          bench "part2" $ whnf part2 inp'
        ]
    ]

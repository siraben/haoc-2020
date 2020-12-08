{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import Control.Monad.State
import Criterion.Main
import qualified Data.IntSet as IS
import Data.List
import qualified Data.Map as M

-- string is nop, acc, jump
type Inst = (String, Int)

type Prog a = State (Int, Int, IS.IntSet, M.Map Int Inst) a

tick :: Prog ()
tick = do
  (pc, a, vs, is) <- get
  put (pc + 1, a, vs, is)

acc :: Int -> Prog ()
acc n = do
  (pc, a, vs, is) <- get
  put (pc + 1, a + n, vs, is)

mark :: Int -> Prog ()
mark x = do
  (pc, a, vs, is) <- get
  put (pc, a, IS.insert x vs, is)

jmp :: Int -> Prog ()
jmp n = do
  (pc, a, vs, is) <- get
  put (pc + n, a, vs, is)

data PS = Loop | Exit deriving (Show, Eq)

exec :: Prog (Int, PS)
exec = do
  (pc, a, vs, is) <- get
  if pc == M.size is
    then pure (a, Exit)
    else do
      let i = is M.! pc
      if pc `IS.member` vs
        then pure (a, Loop)
        else do
          mark pc
          case i of
            ("nop", _) -> tick
            ("acc", n) -> acc n
            ("jmp", n) -> jmp n
          exec

execInit p is = evalState p (0, 0, mempty, is)

part1 = execInit exec
part2 (inp', prog) = filter ((== Exit) . snd) (execInit exec <$> (nopProgs <> jmpProgs))
  where
    nops = findIndices ((== "nop") . fst) prog
    jmps = findIndices ((== "jmp") . fst) prog
    nopProgs = [M.insert i ("jmp", a) inp' | i <- nops, let (_, a) = inp' M.! i]
    jmpProgs = [M.insert i ("nop", a) inp' | i <- jmps, let (_, a) = inp' M.! i]

main = do
  let dayNumber = 8 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- (splitAt 3 <$>) . lines <$> readFile dayFilename
  let prog = map (\(x, y) -> (x, read y :: Int)) inp
  let inp' = M.fromList (zip [0 ..] prog)
  print (part1 inp')
  print (part2 (inp', prog))
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp',
          bench "part2" $ whnf part2 (inp', prog)
        ]
    ]

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.State
import Criterion.Main
import qualified Data.IntSet as IS
import Data.List
import qualified Data.Map as M

-- string is nop, acc, jump
type Inst = (String, Int)

data ProgState = ProgState {pc :: !Int, a :: !Int, vs :: IS.IntSet, is :: M.Map Int Inst}

type Prog a = State ProgState a

data HaltState = Loop | Exit deriving (Show, Eq)

tick :: Prog ()
tick = modify (\s -> s {pc = pc s + 1})

acc :: Int -> Prog ()
acc n = modify (\s -> s {pc = pc s + 1, a = a s + n})

mark :: Int -> Prog ()
mark x = modify (\s -> s {vs = IS.insert x (vs s)})

jmp :: Int -> Prog ()
jmp n = modify (\s -> s {pc = pc s + n})

exec :: Prog (Int, HaltState)
exec = do
  ProgState {pc, a, vs, is} <- get
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

execInit p is = evalState p (ProgState 0 0 mempty is)

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
  inp <- lines <$> readFile dayFilename
  let prog = map ((\[x, y] -> (x, read (dropWhile (== '+') y) :: Int)) . words) inp
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

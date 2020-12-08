{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.State.Strict
import Criterion.Main
import qualified Data.IntSet as IS
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

part2 inp' = head [a | is' <- nopProgs, let (a, s) = execInit exec is', s == Exit]
  where
    change "nop" = "jmp"
    change "jmp" = "nop"
    change a = a
    nopProgs = [M.update (\(x, y) -> Just (change x, y)) i inp' | i <- [0 .. M.size inp' - 1]]

main = do
  let dayNumber = 8 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  let prog = map ((\[x, y] -> (x, read (dropWhile (== '+') y) :: Int)) . words) inp
  let inp' = M.fromList (zip [0 ..] prog)
  print (part1 inp')
  print (part2 inp')
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp',
          bench "part2" $ whnf part2 inp'
        ]
    ]

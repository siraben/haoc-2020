{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import Criterion.Main
import Data.Bits
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Function
import qualified Data.Graph as G
import qualified Data.IntSet as IS
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.State

-- Slow splitOn for prototyping
splitOn :: String -> String -> [String]
splitOn sep s = T.unpack <$> T.splitOn (T.pack sep) (T.pack s)

-- ByteString splitOn
splitOn' :: B.ByteString -> B.ByteString -> [B.ByteString]
splitOn' del bs = go bs
  where
    n = B.length del
    go bs = case B.breakSubstring del bs of
      (ls, rest) ->
        if B.null rest
          then ls : mempty
          else ls : splitOn' del (B.drop n rest)

part1, part2 :: _ -> Int
part1 i = undefined
part2 i = undefined

type Arg = Int
-- string is nop, acc, jump
-- data Inst = Inst String Arg
type Inst = (String, Int)
type Prog a = State (Int, Int, M.Map Int (Inst,Bool)) a

tick :: Prog ()
tick = do
  (pc,a,is) <- get
  put (pc + 1, a,is)

acc n = do
  (pc,a,is) <- get
  put (pc + 1, a + n,is)

mark x = do
  (pc, a,is) <- get
  let (i,b) = is M.! x
  put (pc, a,M.insert x (i,True) is)

jmp n = do
  (pc,a,is) <- get
  put (pc + n,a,is)

data PS = Loop | Exit deriving Show
exec :: Prog (Int, PS)
exec = do
  (pc, a,is) <- get
  if (pc == M.size is)
  then (pure (a,Exit))
  else do
    let (i,b) = is M.! pc
    if b then pure (a,Loop) else do
              mark pc
              case i of
               ("nop", _) -> tick *> exec
               ("acc", n) -> acc n *> exec
               ("jmp", n) -> jmp n *> exec

execInit p is = evalState p (0,0,is)
main = do
  let dayNumber = 8 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- (splitAt 3 <$>) . lines <$> readFile dayFilename
  let prog = (map (\(x,y) -> ((x, read y :: Int),False)) inp)
  let inp' = M.fromList (zip [0..] prog)
  let nops = (findIndices ((== "nop") . fst . fst) prog)
  let jmps = (findIndices ((== "jmp") . fst . fst) prog)
  let nopProgs = [M.insert i (("jmp",a), False) inp'| i <- nops, let (s,a) = fst (inp' M.! i)]
  let jmpProgs = [M.insert i (("nop",a), False) inp'| i <- jmps, let (s,a) = fst (inp' M.! i)]
  print (execInit exec <$> nopProgs)
  print (execInit exec <$> jmpProgs)
  print 4
  -- print nops
  -- print jmps
  -- print (execInit exec inp')
  -- print (part1 inp)
  -- print (part2 inp)
  -- defaultMain
  --   [ bgroup
  --       dayString
  --       [ bench "part1" $ whnf part1 inp,
  --         bench "part2" $ whnf part2 inp
  --       ]
  --   ]

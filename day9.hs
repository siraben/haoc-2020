{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -fdefer-typed-holes #-}

-- import Criterion.Main
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

-- 27911108
subl a b l = [l !! i | i <- [a..b]]

main = do
  let dayNumber = 9
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  let inp' = read <$> inp :: [Int]
  let bbb = iterate (drop 1) inp'
  let bing l = splitAt 25 l
  -- let aaa = drop 25 inp'
  let isValid p n = not (null [(x,y) | x <- p, y <- p, x + y == n])
  -- print inp'
  let x = (filter (not . (\(p,r) -> isValid p (head r))) (bing <$> bbb))
  print (head (snd (head x)))
  let n = length inp'
  let segs = concatMap tails . inits
  print ((\l -> minimum l + maximum l) <$> (filter ((== 27911108) . sum) (segs inp')))
  -- print [(a,b) | a <- [0..length inp - 1], b <- [a..length inp - 1], let n = subl a b inp', sum n == 27911108]
  --- print (head (filter (not . isValid)  aaa))
  -- print (part1 inp)
  -- print (part2 inp)
  -- defaultMain
  --   [ bgroup
  --       dayString
  --       [ bench "part1" $ whnf part1 inp,
  --         bench "part2" $ whnf part2 inp
  --       ]
  --   ]

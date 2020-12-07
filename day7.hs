{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
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
import Text.ParserCombinators.Parsec
import Data.Either

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

tok p = p <* space
word = tok (many letter)

ident = tok . string

nat :: Parser Int
nat = read <$> many1 digit

colPair = do
  [a,b] <- count 2 word
  pure $ concat [a,"-",b]
                
parseLine :: Parser _
parseLine = do
  source <- colPair
  ident "bags"
  ident "contain"
  things <- (string "no other bags" *> pure []) <|> ((tok nat *> colPair <* (try (string "bags") <|> string "bag")) `sepBy` (ident ","))
  string "."
  pure (source, source, things)
  
doit = parse parseLine ""
main = do
  let dayNumber = 7 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  let inp' = undefined inp
  let sample = take 10 inp
  print (take 10 inp)
  -- let pres = (traverse doit inp)
  -- print pres
  let Right pres = (traverse doit inp)
  let (res,_,f) = G.graphFromEdges pres
  let Just sg = f "shiny-gold" -- lookup shiny gold
  let vs = G.vertices res
  print (length [v | v <- vs, v /= sg, G.path res v sg])
  -- print res
  
  -- print (take 10 inp')
  -- print (part1 inp)
  -- print (part2 inp)
  -- defaultMain
  --   [ bgroup
  --       dayString
  --       [ bench "part1" $ whnf part1 inp,
  --         bench "part2" $ whnf part2 inp
  --       ]
  --   ]

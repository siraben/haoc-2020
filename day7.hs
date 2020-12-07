{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import Criterion.Main
import qualified Data.ByteString.Char8 as B
import Data.Function
import Data.Functor
import qualified Data.Graph as G
import Text.Parsec
import qualified Text.Parsec.ByteString as BT

tok p = p <* space

word = tok (many letter)

ident = tok . string

nat :: BT.Parser Int
nat = read <$> many1 digit

colPair = do
  [a, b] <- count 2 word
  pure $ concat [a, "-", b]

newtype Wat = W (Int, String) deriving (Show)

unW (W x) = x

instance Eq Wat where
  (==) = (==) `on` (snd . unW)

instance Ord Wat where
  compare = compare `on` (snd . unW)

parseLine :: BT.Parser _
parseLine = do
  source <- colPair <* ident "bags contain"
  things <-
    (string "no other bags" $> [])
      <|> ((((,) <$> tok nat <*> colPair) <* (string "bag" <* optional (string "s"))) `sepBy` ident ",")
  string "."
  pure (source, W (0, source), W <$> things)

doit = parse parseLine ""

part1 :: [(node, Wat, [Wat])] -> Int
part1 pres = length (G.reachable (G.transposeG g) sg) - 1
  where
    (g, _, f) = G.graphFromEdges pres
    Just sg = f (W (1, "shiny-gold"))

part2 :: [(a, Wat, [Wat])] -> Int
part2 pres = bagsContainedBy i (W (1, "shiny-gold")) - 1
  where
    i = G.graphFromEdges pres

-- g, the graph itself
-- h, function from vertex to (node, key, [key])
-- f, key -> Maybe vertex
bagsContainedBy i@(_, h, f) x@(W (n, _)) = n + sum (map recurse cs)
  where
    -- Current vertex number
    Just v = f x
    -- Children
    (_, _, cs) = h v
    recurse (W (m, s)) = bagsContainedBy i (W (n * m, s))

main = do
  let dayNumber = 7 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- B.lines <$> B.readFile dayFilename
  let Right pres = traverse doit inp
  print (part1 pres)
  print (part2 pres)
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 pres,
          bench "part2" $ whnf part2 pres
        ]
    ]
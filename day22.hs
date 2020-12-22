{-# LANGUAGE BangPatterns #-}

import qualified Data.Text as T
import Criterion.Main

-- Slow splitOn for prototyping
splitOn :: String -> String -> [String]
splitOn sep s = T.unpack <$> T.splitOn (T.pack sep) (T.pack s)

-- | Repeat a function until you get the same result twice.
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint = fixedPointBy (==)

-- | Repeat a function until some condition is met.
fixedPointBy :: (a -> a -> Bool) -> (a -> a) -> a -> a
fixedPointBy cmp f = go
  where
    go !x
      | x `cmp` y = x
      | otherwise = go $! y
      where
        y = f $! x

-- Start working down here
type GS = ([Int], [Int])

step :: GS -> GS
step (a : as, b : bs)
  | a > b = (as ++ l, bs)
  | a < b = (as, bs ++ l)
  where
    l = [max a b, min a b]
step x = x

calcScore l = sum (zipWith (*) [1 ..] (reverse l))

calcWin ([], l) = calcScore l
calcWin (l, []) = calcScore l

part1 (a, b) = calcWin (fixedPoint step (a, b))

type GH = [GS]

{-

(1) Before either player deals a card, if there was a previous round
    in this game that had exactly the same cards in the same order in
    the same players' decks, the game instantly ends in a win for
    player 1. Previous rounds from other games are not
    considered. (This prevents infinite games of Recursive Combat,
    which everyone agrees is a bad idea.)

(2) Otherwise, this round's cards must be in a new configuration; the
    players begin the round by each drawing the top card of their deck
    as normal.

(3) If both players have at least as many cards remaining in their
    deck as the value of the card they just drew, the winner of the
    round is determined by playing a new game of Recursive Combat (see
    below).

(4) Otherwise, at least one player must not have enough cards left in
    their deck to recurse; the winner of the round is the player with
    the higher-value card.

-}
--     in prog, one, two
data S = P | O | T deriving (Show, Eq)

step2 :: (GS, GH, S) -> (GS, GH, S)
step2 (g@([], b), h, _) = (g, h, T)
step2 (g@(a, []), h, _) = (g, h, O)
step2 (g@(a : as, b : bs), h, s)
  | g `elem` h = (g, h, O) -- (1)
  | a <= length as && b <= length bs =
    let l = case s of
          O -> [a, b]
          T -> [b, a]
        (_, _, s) = fixedPoint step2 ((take a as, take b bs), [], P)
     in case s of -- (3)
          T -> ((as, bs ++ l), g : h, P)
          O -> ((as ++ l, bs), g : h, P)
  | otherwise = (step g, g : h, s) -- (2), (4)

part2 (a, b) = calcWin (let (x, _, _) = fixedPointBy f step2 ((a, b), [], P) in x)
  where
    f (_,_,s1) (_,_,s2) = s1 /= s2

main = do
  let dayNumber = 22
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- (tail . lines <$>) . splitOn "\n\n" <$> readFile dayFilename
  let [a, b] = map (map read) inp :: [[Int]]
  let inp' = (a,b)
  print (part1 inp')
  print (part2 inp')
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp'
--          bench "part2" $ whnf part2 inp'
        ]
    ]
  

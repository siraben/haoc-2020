{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists #-}

import Criterion.Main
import Data.Foldable
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Q
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as UVector
import Data.Bifunctor

type UVector = UVector.Vector

splitOn :: String -> String -> [String]
splitOn sep s = T.unpack <$> T.splitOn (T.pack sep) (T.pack s)

-- Start working down here
step1 (Empty, b) = b
step1 (a, Empty) = a
step1 (a :<| as, b :<| bs)
  | a > b = step1 (as <> l, bs)
  | a < b = step1 (as, bs <> l)
  where
    l = Q.fromList [max a b, min a b]

calcScore = sum . zipWith (*) [1 ..] . reverse . toList

part1 = calcScore . step1

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

step2 :: Set (UVector Int) -> Deck -> Deck -> Either Deck Deck
step2 _ Empty xs = Right xs
step2 _ xs Empty = Left xs
step2 seen xxs@(x :<| xs) yys@(y :<| ys)
  | here `elem` seen = Left xs
  | x <= Q.length xs && y <= Q.length ys =
      case step2 S.empty (Q.take x xs) (Q.take y ys) of
        Left  _ -> step2 seen1 (give xs x y) ys
        Right _ -> step2 seen1 xs (give ys y x)
  | x > y = step2 seen1 (give xs x y) ys
  | otherwise = step2 seen1 xs (give ys y x)
  where
    here = characterize xxs yys
    seen1 = S.insert here seen

type Deck = Seq Int

characterize :: Deck -> Deck -> UVector Int
characterize xs ys = V.fromList (toList xs ++ [-1] ++ toList ys)

give :: Deck -> Int -> Int -> Deck
give a b c = a Q.|> b Q.|> c

part2 = bimap calcScore calcScore .  uncurry (step2 S.empty)

main = do
  let dayNumber = 22
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- (tail . lines <$>) . splitOn "\n\n" <$> readFile dayFilename
  let [a, b] = map (map read) inp :: [[Int]]
  let inp' = (Q.fromList a, Q.fromList b)
  print (part1 inp')
  print (part2 inp')
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp',
          bench "part2" $ whnf part2 inp'
        ]
    ]

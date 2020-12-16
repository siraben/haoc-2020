{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Ix
import Data.List
import Data.Maybe
import qualified Text.ParserCombinators.ReadP as P

type Range = (Int, Int)

cR :: Range -> Range -> (Int -> Bool)
cR (l1, h1) (l2, h2) n = inRange (l1, h1) n || inRange (l2, h2) n

ranges =
  [ (25, 80) `cR` (90, 961),
    (41, 133) `cR` (148, 968),
    (48, 425) `cR` (451, 952),
    (25, 371) `cR` (384, 966),
    (49, 531) `cR` (546, 973),
    (45, 641) `cR` (656, 954),
    (43, 357) `cR` (364, 969),
    (40, 669) `cR` (689, 954),
    (40, 550) `cR` (570, 956),
    (49, 854) `cR` (863, 953),
    (48, 601) `cR` (614, 964),
    (27, 698) `cR` (715, 962),
    (38, 781) `cR` (800, 970),
    (47, 824) `cR` (842, 965),
    (45, 219) `cR` (241, 955),
    (47, 388) `cR` (401, 954),
    (42, 906) `cR` (919, 965),
    (40, 726) `cR` (733, 955),
    (27, 161) `cR` (174, 974),
    (48, 103) `cR` (110, 954)
  ]

rangesA = zip [0 ..] ranges

myTicket :: [Int]
myTicket = [181, 131, 61, 67, 151, 59, 113, 101, 79, 53, 71, 193, 179, 103, 149, 157, 127, 97, 73, 191]

aaaa :: Int -> [Int]
aaaa x = [n | (n, f) <- rangesA, f x]

parseLine :: P.ReadP [Int]
parseLine = P.sepBy (P.readS_to_P reads) (P.string ",") <* P.string "\n"

backsol :: (Int, ([[Int]], IntSet)) -> (Int, ([[Int]], IntSet))
backsol (n, (l, solset)) = (head $ IS.elems $ sols IS.\\ solset, (l', sols))
  where
    sols = IS.fromList . concat . filter ((== 1) . length) $ l'
    l' = map f l
    f l = if length l == 1 then l else filter (/= n) l

part1 :: [[Int]] -> Int
part1 inp' = sum (concatMap (filter (\f -> not (any ($ f) ranges))) inp')

part2 :: [[Int]] -> Int
part2 validTickets = product ((myTicket !!) <$> (fromJust . (`elemIndex` answer) <$> [0 .. 5]))
  where
    (_, (answer', _)) = iterate backsol (init, (constraints, IS.singleton init)) !! 19
    answer = concat answer'
    constraints = foldl1' intersect <$> transpose (map aaaa <$> validTickets)
    init = head . concat . filter ((== 1) . length) $ constraints

main = do
  let dayNumber = 16
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- readFile dayFilename
  let (inp', "") = last (P.readP_to_S (P.many parseLine) inp)
  let isValidTicket l = and [any ($ f) ranges | f <- l]
  let validTickets = filter isValidTicket inp'
  print (part1 inp')
  print (part2 validTickets)
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp',
          bench "part2" $ whnf part2 validTickets
        ]
    ]

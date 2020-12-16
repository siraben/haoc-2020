{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -fdefer-typed-holes -fno-warn-unused-imports #-}

import qualified Data.IntSet as IS
import Data.List
import Data.Maybe
import qualified Text.ParserCombinators.ReadP as P

-- Start working down here
part1, part2 :: _ -> Int
part1 i = undefined
part2 i = undefined

-- parseInput =
data Range = Range Int Int deriving (Show)

cR :: Range -> Range -> (Int -> Bool)
cR (Range l1 h1) (Range l2 h2) n = within n l1 h1 || within n l2 h2
  where
    within n a b = a <= n && n <= b

ranges =
  [ Range 25 80 `cR` Range 90 961,
    Range 41 133 `cR` Range 148 968,
    Range 48 425 `cR` Range 451 952,
    Range 25 371 `cR` Range 384 966,
    Range 49 531 `cR` Range 546 973,
    Range 45 641 `cR` Range 656 954,
    Range 43 357 `cR` Range 364 969,
    Range 40 669 `cR` Range 689 954,
    Range 40 550 `cR` Range 570 956,
    Range 49 854 `cR` Range 863 953,
    Range 48 601 `cR` Range 614 964,
    Range 27 698 `cR` Range 715 962,
    Range 38 781 `cR` Range 800 970,
    Range 47 824 `cR` Range 842 965,
    Range 45 219 `cR` Range 241 955,
    Range 47 388 `cR` Range 401 954,
    Range 42 906 `cR` Range 919 965,
    Range 40 726 `cR` Range 733 955,
    Range 27 161 `cR` Range 174 974,
    Range 48 103 `cR` Range 110 954
  ]

rangesA = zip [0 ..] ranges

myTicket :: [Int]
myTicket = [181, 131, 61, 67, 151, 59, 113, 101, 79, 53, 71, 193, 179, 103, 149, 157, 127, 97, 73, 191]

aaaa :: Int -> [Int]
aaaa x = [n | (n, f) <- rangesA, f x]

parseLine :: P.ReadP [Int]
parseLine = P.sepBy (P.readS_to_P reads) (P.string ",") <* P.string "\n"

backsol (n, (l, solset)) = (head $ IS.toList $ sols IS.\\ solset, (l', sols))
  where
    sols = IS.fromList . concat . filter ((== 1) . length) $ l'
    l' = map f l
    f l = if length l == 1 then l else filter (/= n) l

main = do
  let dayNumber = 16
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- readFile dayFilename
  let (fo, "") = last (P.readP_to_S (P.many parseLine) inp)
  let isValidTicket l = and [any ($ f) ranges | f <- l]
  let inp' = filter isValidTicket fo
  let constraints = foldl1 intersect <$> transpose (map aaaa <$> inp')
  let init = head . concat . filter ((== 1) . length) $ constraints
  let (_, (answer', _)) = iterate backsol (init, (constraints, IS.singleton init)) !! 19
  let answer = concat answer'
  print (sum (fo >>= (\l -> [f | f <- l, not (any ($ f) ranges)])))
  print (product ((myTicket !!) <$> (fromJust . (`elemIndex` answer) <$> [0 .. 5])))

-- print (part1 inp)
-- print (part2 inp)
-- defaultMain
--   [ bgroup
--       dayString
--       [ bench "part1" $ whnf part1 inp,
--         bench "part2" $ whnf part2 inp
--       ]
--   ]

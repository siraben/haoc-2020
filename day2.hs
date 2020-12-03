import Criterion.Main
import Text.ParserCombinators.Parsec

nat :: Parser Int
nat = read <$> many1 digit

tok = (<* space)

--        lo   hi   char pass
type L = (Int, Int, Char, String)

processLine = (,,,) <$> (nat <* char '-') <*> (nat <* space) <*> (letter <* tok (char ':')) <*> many letter

isValid1 :: L -> Bool
isValid1 (l, h, c, s) = l <= occurs && occurs <= h
  where
    occurs = length (filter (== c) s)

isValid2 :: L -> Bool
isValid2 (l, h, c, s) = (c == s !! (h - 1)) /= (c == s !! (l - 1))

part1 = length . filter isValid1
part2 = length . filter isValid2

main = do
  inp <- lines <$> readFile "day2.txt"
  let Right x = sequenceA (parse processLine "" <$> inp)
  putStr "Part 1: "
  print (part1 x)
  putStr "Part 2: "
  print (part2 x)
  defaultMain [ bgroup "day2" [ bench "part1" $ whnf part1 x
                              , bench "part2" $ whnf part2 x
                              ] ]
 

{-
From ##adventofcode-spoilers
iqubic: hmm. Not sure I like traversing the list twice in "length (filter p xs)"
siraben: Is it really traversing twice?
siraben: List fusion should take over
iqubic: I was not aware.
dsal: I did basically that. The input's too small to matter.
siraben: Right.
siraben: Well one could fuse it manually via a fusion law

Proof.
  length . filter p
= foldr (\_ c -> c + 1) 0 . filter p
= foldr (\_ c -> c + 1) 0  . foldr (\x acc -> if p x then x:acc else acc) []

Foldr fusion
 h w = v and h (g x y) = f x (h y)
-----------------------------------
  h . foldr g w = foldr f v

So,
  h = length = foldr (\_ c -> c + 1) 0
  foldr g w = foldr (\x acc -> if p x then x:acc else acc) []
  g = (\x acc -> if p x then x:acc else acc)
  w = []

Now,
  h w = length [] = 0
  So v = 0.

  h (g x y) = f x (h y)
= length ((\x acc -> if p x then x:acc else acc) x y) = f x (length y)
= length (if p x then x:y else y) = f x (length y)
= if p x then length (x:y) else length y = f x (length y)
= if p x then 1 + length y else length y = f x (length y)
<= { generalize length y to ys }
= if p x then 1 + ys else ys = f x ys

So,
  f x ys = if p x then 1 + ys else ys

So
  length . filter p
= foldr (\_ c -> c + 1) 0  . foldr (\x acc -> if p x then x:acc else acc) []
= foldr (\x ys -> if p x then 1 + ys else ys) 0
QED
Next,
  foldr (\x ys -> if p x then 1 + ys else ys) 0
= foldl (\ys x -> if p x then 1 + ys else ys) 0
  { for finite list }
= foldl' (\ys x -> if p x then 1 + ys else ys) 0
-}

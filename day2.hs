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

main = do
  inp <- lines <$> readFile "day2.txt"
  let Right x = sequenceA (parse processLine "" <$> inp)
  putStr "Part 1: "
  print (length (filter isValid1 x))
  putStr "Part 2: "
  print (length (filter isValid2 x))

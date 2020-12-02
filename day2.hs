import Text.ParserCombinators.Parsec

tok :: Parser a -> Parser a
tok p = p <* space

nat :: Parser Int
nat = read <$> many1 digit

--           lo   hi   char pass
type Line = (Int, Int, Char, String)

processLine = do
  lo <- nat
  char '-'
  hi <- tok nat
  c <- letter <* tok (char ':')
  p <- many letter
  pure (lo,hi,c,p)

isValid1 :: (Int, Int, Char, String) -> Bool
isValid1 (l,h,c,s) = l <= occurs && occurs <= h
  where
    occurs = length (filter (== c) s)

isValid2 :: (Int, Int, Char, String) -> Bool
isValid2 (l,h,c,s) = (c == s !! (h - 1)) /= (c == s !! (l - 1))

main = do
  inp <- lines <$> readFile "day2.txt"
  let Right x = sequenceA (parse processLine "" <$> inp)
  putStr "Part 1: "
  print (length (filter isValid1 x))
  putStr "Part 2: "
  print (length (filter isValid2 x))



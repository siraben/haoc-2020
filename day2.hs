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

isValid :: (Int, Int, Char, String) -> Bool
isValid (l,h,c,s) = l <= occurs && occurs <= h
  where
    occurs = length (filter (== c) s)

main = do
  inp <- lines <$> readFile "day2.txt"
  let Right x = traverse id (parse processLine "" <$> inp)
  print (length (filter isValid x))
  
  

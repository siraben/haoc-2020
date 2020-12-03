
part1 i = length (filter (== '#') (head <$> slope))
  where
    w = cycle <$> i
    slope = tail $ zipWith ($) (iterate (drop 3 .) id) w

part2 i = product ((`solveGeneric` i) <$> [1,3,5,7]) * hack
  where
    hack = length (filter (== '#') (head <$> slope))
    w = everySecond (cycle <$> i)
    slope = tail $ zipWith ($) (iterate (drop 1 .) id) w

-- right down input
solveGeneric r i = length (filter (== '#') (head <$> slope))
  where
    w = cycle <$> i
    slope = tail $ zipWith ($) (iterate (drop r .) id) w


everySecond (a:_:as) = a : everySecond as
everySecond [a] = [a]
everySecond [] = []

-- part2 
main = do
  inp <- lines <$> readFile "day3.txt"
  print (part1 inp)
  print (part2 inp)

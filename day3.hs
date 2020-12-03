import qualified Data.Text as T
import qualified Data.Text.IO as TIO

part1 :: [T.Text] -> Int
part1 = solveGeneric 3

-- right down input
solveGeneric :: Int -> [T.Text] -> Int
solveGeneric r i = length (filter (== '#') slope)
  where
    cols = iterate ((`mod` 31) . (+ r)) 0
    slope = zipWith T.index i cols

part2 :: [T.Text] -> Int
part2 i = product ((`solveGeneric` i) <$> [1, 3, 5, 7]) * solveGeneric 1 (everySecond i)

everySecond (a : _ : as) = a : everySecond as
everySecond [a] = [a]
everySecond [] = []

main = do
  inp <- T.lines <$> TIO.readFile "day3.txt"
  print (part1 inp)
  print (part2 inp)

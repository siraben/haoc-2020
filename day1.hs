import qualified Data.Set as S

sample :: [Int]
sample = [1721, 979, 366, 299, 675, 1456]

solve1 :: Int -> [Int] -> S.Set Int -> Int
solve1 t l s = head [(2020 - n) * n | n <- l, S.member (t - n) s]

solve2 l s = head [n * c | n <- l, c <- solve1' (2020 - n) l s ]
  where
    solve1' t l s = [(t - n) * n | n <- l, S.member (t - n) s]

-- Challenge: how to generalize to arbitrary n?

main = do
  inp <- (read <$>) . lines <$> readFile "day1.txt"
  let s = S.fromList inp
  putStr "Part 1: "
  print (solve1 2020 inp s)
  putStr "Part 2: "
  print (solve2 inp s)

import qualified Data.Set as S

solve :: [Int] -> Int
solve l = head [(2020 - n) * n | n <- l, S.member (2020 - n) (S.fromList l)]

main = (print . solve) . ((read <$>) . lines) =<< readFile "day1.txt"

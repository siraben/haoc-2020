import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Solve for right r down d
solve i r d = let (a,_,_) = foldl' f (0, r, d) i in a
  where
    -- (number of #s, col counter, row counter)
    f (hs, cc, 0) l = (hs + if l `T.index` cc == '#' then 1 else 0, (cc + r) `mod` 31, d - 1)
    f (hs, cc, dc) _ = (hs, cc, dc - 1)

part1, part2 :: [T.Text] -> Int
part1 i = solve i 3 1
part2 i = product (uncurry (solve i) <$> [(1,1), (3,1), (5,1), (7,1), (1,2)])

main = do
  inp <- T.lines <$> TIO.readFile "day3.txt"
  print (part1 inp)
  print (part2 inp)

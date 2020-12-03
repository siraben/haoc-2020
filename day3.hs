{-# LANGUAGE BangPatterns #-}
import Data.List (foldl')
import qualified Data.ByteString.Char8 as B
import Criterion.Main

-- Solve for right r down d
solve i r d = let (a,_,_) = foldl' f (0, r, d) i in a
  where
    -- (number of #s, col counter, row counter)
    f (!hs, !cc, 0) l = (hs + if l `B.index` cc == '#' then 1 else 0, (cc + r) `mod` 31, d - 1)
    f (!hs, !cc, !dc) _ = (hs, cc, dc - 1)

part1, part2 :: [B.ByteString] -> Int
part1 i = solve i 3 1
part2 i = solve i 1 1 * solve i 3 1 * solve i 5 1 * solve i 7 1 * solve i 1 2

main = do
  inp <- B.lines <$> B.readFile "day3.txt"
  print (part1 inp)
  print (part2 inp)
  defaultMain [ bgroup "day3" [ bench "part1" $ whnf part1 inp
                              , bench "part2" $ whnf part2 inp
                              ] ]

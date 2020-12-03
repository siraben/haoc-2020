{-# LANGUAGE BangPatterns #-}
import Criterion.Main
import qualified Data.ByteString.Char8 as B
import Data.List (foldl')

-- Solve for right r down d
part1, part2 :: [B.ByteString] -> Int
part1 i = fst (foldl' f (0, 3) (tail i))
  where
    -- (number of #s, row counter)
    f :: (Int, Int) -> B.ByteString -> (Int, Int)
    f (!hs, !cc) l = (hs + if l `B.index` cc == '#' then 1 else 0, (cc + 3) `mod` 31)
part2 i = let (a, b, c, d, e, _) = foldl' f (0, 0, 0, 0, 0, 1) (tail i) in a * b * c * d * e
  where
    f :: (Int, Int, Int, Int, Int, Int) -> B.ByteString -> (Int, Int, Int, Int, Int, Int)
    f (!a, !b, !c, !d, !e, !cc) l = (a', b', c', d', e', cc')
      where
        g n = if l `B.index` ((cc * n) `mod` 31) == '#' then 1 else 0
        a' = a + g 1
        b' = b + g 3
        c' = c + g 5
        d' = d + g 7
        e' = e + if even cc && l `B.index` ((cc `div` 2) `mod` 31) == '#' then 1 else 0
        cc' = cc + 1

main = do
  inp <- B.lines <$> B.readFile "day3.txt"
  print (part1 inp)
  print (part2 inp)
  defaultMain
    [ bgroup "day3" [ bench "part1" $ whnf part1 inp,
                      bench "part2" $ whnf part2 inp
                    ] ]

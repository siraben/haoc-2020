{-# LANGUAGE BangPatterns, OverloadedStrings #-}
import Criterion.Main
import qualified Data.ByteString.Char8 as B
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List
import Debug.Trace

-- part1, part2 :: [B.ByteString] -> Int
-- part1 i = undefined
-- part2 i = undefined

splitOn' :: B.ByteString -> B.ByteString -> [B.ByteString]
splitOn' _ bs | B.null bs = []
splitOn' del bs =
  case B.breakSubstring del bs of
    (ls, rest) ->
      if B.null rest
        then ls : mempty
        else ls : splitOn' del (B.tail rest)

upHalf (l,h) = (l + ((1 + h - l) `div` 2),h)
loHalf (l,h) = (l,l + ((h - l) `div` 2))


solve :: String -> Int
solve l = r * 8 + c
  where
    (a,b) = splitAt 7 l
    go s 'F' = loHalf s
    go s 'B' = upHalf s
    r = fst (foldl' (\s c -> id (go s c)) (0,127 ::Int) a)
    c = fst (foldl' fish (0,7) b)
    fish s 'L' = loHalf s
    fish s 'R' = upHalf s

main = do
  let dayNumber = 5 :: Int -- FIXME: change day number
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  let dat = (solve <$> inp)
  print (maximum dat)
  print ([54..930] \\ dat)
  -- print (part1 inp)
  -- print (part2 inp)
  -- defaultMain [ bgroup dayString [ bench "part1" $ whnf part1 inp
  --                                , bench "part2" $ whnf part2 inp
  --                                ] ]

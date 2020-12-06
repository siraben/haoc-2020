{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- import Criterion.Main
import qualified Data.ByteString.Char8 as B
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List as L
part1, part2 :: [B.ByteString] -> Int
part1 i = undefined
part2 i = undefined


splitOn' :: B.ByteString -> B.ByteString -> [B.ByteString]
splitOn' _ bs | B.null bs = []
splitOn' del bs =
  case B.breakSubstring del bs of
    (ls, rest) ->
      if B.null rest
        then ls : mempty
        else ls : splitOn' del (B.tail rest)


main = do
  let dayNumber = 6 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- B.readFile dayFilename
  let f = length . B.group . B.sort
  let inp' = B.words . B.unwords . B.lines <$> splitOn' "\n\n" inp
  let inp'' = f . B.concat <$> inp'
  let inp''' = length . foldr L.intersect ['a'..'z'] <$> ((B.unpack <$>) <$> inp')
  print (sum inp'')
  print (sum inp''')
  -- print (part2 inp)
  -- defaultMain [ bgroup dayString [ bench "part1" $ whnf part1 inp
  --                                , bench "part2" $ whnf part2 inp
  --                                ] ]

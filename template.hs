{-# LANGUAGE BangPatterns, OverloadedStrings #-}
import Criterion.Main
import Data.Bits
import qualified Data.ByteString.Char8 as B
import Data.Char
import qualified Data.IntSet as IS
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

part1, part2 :: [B.ByteString] -> Int
part1 i = undefined
part2 i = undefined

-- Slow splitOn for prototyping
splitOn :: String -> String -> [String]
splitOn sep s = T.unpack <$> T.splitOn (T.pack sep) (T.pack s)

-- ByteString splitOn
splitOn' :: B.ByteString -> B.ByteString -> [B.ByteString]
splitOn' del bs = go bs
  where
    n = B.length del
    go bs = case B.breakSubstring del bs of
              (ls, rest) ->
                if B.null rest
                  then ls : mempty
                  else ls : splitOn' del (B.drop n rest)

main = do
  let dayNumber = undefined :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- B.lines <$> B.readFile dayFilename
  print (part1 inp)
  print (part2 inp)
  defaultMain [ bgroup dayString [ bench "part1" $ whnf part1 inp
                                 , bench "part2" $ whnf part2 inp
                                 ] ]

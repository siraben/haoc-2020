{-# LANGUAGE BangPatterns, OverloadedStrings #-}
import Criterion.Main
import qualified Data.ByteString.Char8 as B
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

part1, part2 :: [B.ByteString] -> Int
part1 i = undefined
part2 i = undefined

splitOn' del bs = go bs
  where
    n = B.length del
    go bs = case B.breakSubstring del bs of
              (ls, rest) ->
                if B.null rest
                  then ls : mempty
                  else ls : splitOn' del (B.drop n rest)

main = do
  let dayNumber = undefined :: Int -- FIXME: change day number
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- B.lines <$> B.readFile dayFilename
  print (part1 inp)
  print (part2 inp)
  defaultMain [ bgroup dayString [ bench "part1" $ whnf part1 inp
                                 , bench "part2" $ whnf part2 inp
                                 ] ]

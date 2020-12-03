{-# LANGUAGE BangPatterns #-}
import Criterion.Main
import qualified Data.ByteString.Char8 as B

part1, part2 :: [B.ByteString] -> Int
part1 i = undefined
part2 i = undefined

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

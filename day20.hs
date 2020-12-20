{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Criterion.Main
import Data.Foldable
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.Text as T

-- Slow splitOn for prototyping
splitOn :: String -> String -> [String]
splitOn sep s = T.unpack <$> T.splitOn (T.pack sep) (T.pack s)

-- | Count the number of items in a container where the predicate is true.
countTrue :: Foldable f => (a -> Bool) -> f a -> Int
countTrue p = length . filter p . toList

header s = read (init (drop 5 s)) :: Int

toBinNum = foldl' f 0
  where
    f n '#' = n * 2 + 1
    f n '.' = n * 2

mkBlock (h : d) = (header h, toBinNum <$> ([id, reverse] <*> [top, bot, left, right]))
  where
    -- consider just the edges
    top = head d
    bot = last d
    left = head <$> d
    right = last <$> d

-- is s unique wrt. all sides l
uniqueSide :: Int -> IntMap Int -> Bool
uniqueSide s m = IM.alter (fmap pred) s m IM.! s == 0

-- | Build a frequency map
freqs :: [Int] -> IntMap Int
freqs = IM.fromListWith (+) . map (,1) . toList

part1 inp' = product [x | (x, s) <- inp', countTrue (`uniqueSide` allSides) s == 4]
  where
    allSides = freqs (snd =<< inp')

main = do
  let dayNumber = 20
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- (lines <$>) . splitOn "\n\n" <$> readFile dayFilename
  let inp' = mkBlock <$> inp
  print (part1 inp')
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp'
        ]
    ]

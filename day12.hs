{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main ( defaultMain, bench, bgroup, whnf )
import Data.Foldable ( Foldable(foldl') )

data Orientation = N | E | S | W deriving (Show, Eq, Ord, Enum)

type ShipState = ((Int, Int), Orientation)

move :: ShipState -> (Char, Int) -> ShipState
move ((!x, !y), d) ('N', n) = ((x, y + n), d)
move ((!x, !y), d) ('E', n) = ((x + n, y), d)
move ((!x, !y), d) ('S', n) = ((x, y - n), d)
move ((!x, !y), d) ('W', n) = ((x - n, y), d)
move ((!x, !y), N) ('F', n) = ((x, y + n), N)
move ((!x, !y), E) ('F', n) = ((x + n, y), E)
move ((!x, !y), S) ('F', n) = ((x, y - n), S)
move ((!x, !y), W) ('F', n) = ((x - n, y), W)
move (s, d) ('R', a) = (s, toEnum ((fromEnum d + (a `div` 90)) `mod` 4))
move (s, d) ('L', a) = (s, toEnum ((fromEnum d + (4 - (a `div` 90))) `mod` 4))

type Waypoint = (Int, Int)

type ShipState2 = (ShipState, Waypoint)

rotateRN p 0 = p
rotateRN (x,y) 1 = (y,-x)
rotateRN (x,y) 2 = (-x,-y)
rotateRN (x,y) 3 = (-y,x)
rotateRN p n = rotateRN p (n `mod` 4)

rotateLN p n = rotateRN p (4 - (n `mod` 4))

move2 :: ShipState2 -> (Char, Int) -> ShipState2
move2 (s, (!wx, !wy)) ('N', n) = (s, (wx, wy + n))
move2 (s, (!wx, !wy)) ('E', n) = (s, (wx + n, wy))
move2 (s, (!wx, !wy)) ('S', n) = (s, (wx, wy - n))
move2 (s, (!wx, !wy)) ('W', n) = (s, (wx - n, wy))
move2 (((!x, !y), d), w@(!wx, !wy)) ('F', n) = (((x + wx * n, y + wy * n), d), w)
move2 (s, w) ('R', a) = (s, rotateRN w (a `div` 90))
move2 (s, w) ('L', a) = (s, rotateLN w (a `div` 90))

manDist (x, y) = abs x + abs y

-- Start working down here
part1, part2 :: [(Char, Int)] -> Int
part1 inp = manDist (fst (foldl' move init inp))
  where
    init = ((0, 0), E)
part2 inp = manDist (fst (fst (foldl' move2 init2 inp)))
  where
    init2 = (((0, 0), E), (10, 1))

main = do
  let dayNumber = 12
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  let process (h:t) = (h,read t)
  let inp' = process <$> inp
  print (part1 inp')
  print (part2 inp')
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp',
          bench "part2" $ whnf part2 inp'
        ]
    ]

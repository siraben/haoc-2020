{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

import Criterion.Main
import Data.Foldable
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Text.ParserCombinators.Parsec

-- | Build a frequency map
freqs :: (Foldable f, Ord a) => f a -> Map a Int
freqs = M.fromListWith (+) . map (,1) . toList

--       ing list   allergens
type L = ([String], [String])

tok :: Parser a -> Parser a
tok p = p <* spaces

symb = tok . string

word = many1 letter

parseL :: Parser L
parseL = flip (,) <$> (many1 (tok word) <* string "(contains ") <*> sepBy (tok word) (symb ",")

part1 :: [([String], [String])] -> Int
part1 inp' = sum [allIngredsM M.! i | i <- goodIngreds]
  where
    allergens = concatMap snd inp'
    allIngredsM = freqs allergens
    allIngredsS = S.fromList (M.keys allIngredsM)
    constraints = M.unionsWith S.intersection (uncurry M.singleton <$> (map aa . uncurry ldist =<< inp')) :: Map String (Set String)
    badIngreds = S.unions (M.elems constraints)
    goodIngreds = S.elems (allIngredsS S.\\ badIngreds)
    -- Possible assignments
    aa ls = (fst (head ls), S.fromList (snd <$> ls))
    ldist a i = [(x,) <$> i | x <- a]

findFirst :: Foldable f => (a -> Bool) -> f a -> a
findFirst f = fromJust . find f

delSnd x l = [(a, S.delete x s) | (a, s) <- l]

step :: ((String, [(String, Set String)], [(String, String)]) -> (String, [(String, Set String)], [(String, String)]))
step (_, constraints', sols) = (s, delSnd s constraints', (init, s) : sols)
  where
    (init, S.elems -> [s]) = findFirst ((== 1) . S.size . snd) constraints'

iter :: Int -> ((String, [(String, Set String)], [(String, String)]) -> (String, [(String, Set String)], [(String, String)]))
iter (0 :: Int) x = x
iter !n x = iter (n -1) $! step x

part2 inp' = report (snd <$> sortOn fst sol)
  where
    (_, _, sol) = iter 8 (s, constraints, [] :: [(String, String)])
    report = intercalate ","
    (_, S.elems -> [s]) = findFirst ((== 1) . S.size . snd) constraints
    constraints = M.toList (M.unionsWith S.intersection (uncurry M.singleton <$> (map aa . uncurry ldist =<< inp')))
    aa ls = (fst (head ls), S.fromList (snd <$> ls))
    ldist a i = [(x,) <$> i | x <- a]

main = do
  let dayNumber = 21
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  let Right inp' = traverse (parse parseL "") inp
  print (part1 inp')
  print (part2 inp')
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp',
          bench "part2" $ whnf part2 inp'
        ]
    ]

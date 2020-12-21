{-# LANGUAGE TupleSections #-}

import Data.Foldable
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Text.ParserCombinators.Parsec

swap (a, b) = (b, a)

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

-- Possible assignments
aa ls = (name, S.fromList (snd <$> ls))
  where
    name = fst (head ls)

dd a i = [(x,) <$> i | x <- a]

report = intercalate ","

sol :: [(String, String)]
sol = [("shellfish", "clg"), ("peanuts", "lxjtns"), ("sesame", "vzzz"), ("soy", "cxfz"), ("eggs", "prxmdlz"), ("wheat", "qdfpq"), ("nuts", "knprxg"), ("fish", "ncjv")]

main = do
  let dayNumber = 21
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  let Right inp' = traverse (parse parseL "") inp
  let allIngreds' = freqs (concatMap snd inp')
  let allIngreds = S.fromList (concatMap snd inp')
  let constraints = M.unionsWith S.intersection (uncurry M.singleton <$> (map aa . uncurry dd =<< inp')) :: Map String (Set String)
  let badIngreds = S.unions (M.elems constraints)
  let goodIngreds = S.elems (allIngreds S.\\ badIngreds)
  print (sum [allIngreds' M.! i | i <- goodIngreds])
-- print (part1 inp)
-- print (part2 inp)
-- defaultMain
--   [ bgroup
--       dayString
--       [ bench "part1" $ whnf part1 inp,
--         bench "part2" $ whnf part2 inp
--       ]
--   ]

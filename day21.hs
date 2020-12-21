{-# LANGUAGE TupleSections #-}

import Data.Foldable
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Text.ParserCombinators.Parsec
import Criterion.Main

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

part1 inp' = sum [allIngreds' M.! i | i <- goodIngreds]
  where
    allergens = concatMap snd inp'
    allIngreds' = freqs allergens
    allIngreds = S.fromList (M.keys allIngreds')
    constraints = M.unionsWith S.intersection (uncurry M.singleton <$> (map aa . uncurry dd =<< inp')) :: Map String (Set String)
    badIngreds = S.unions (M.elems constraints)
    goodIngreds = S.elems (allIngreds S.\\ badIngreds)

main = do
  let dayNumber = 21
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  let Right inp' = traverse (parse parseL "") inp
  print (part1 inp')
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp'
        ]
    ]

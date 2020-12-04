{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
import Criterion.Main
import qualified Data.ByteString.Char8 as B
import Data.List
import Text.ParserCombinators.Parsec
import Data.Function
import Control.Monad
import Data.Either

part1, part2 :: [String] -> Int
part1 i = undefined
part2 i = undefined

splitBy delimiter = foldr f [[]]
            where f c l@(x:xs) | c == delimiter = []:l
                             | otherwise = (c:x):xs

check l = isValid l'
  where
    l' = takeWhile (/= ':') <$> l
    allLabels = sort ["eyr","iyr","byr","ecl","pid","hcl","hgt","cid"]
    isValid ls = allLabels == sort ls || (allLabels \\ ["cid"]) == sort ls


type EyeCol = String
type PassNo = String -- nine digits


tok :: Parser a -> Parser a
tok = (<* space)
-- parse labelled field
parseLf l p = do
  string l
  string ":"
  tok p

nat :: Parser Int
nat = read <$> many1 digit

word = tok (many letter)

hgtParse = do
  let failIfNot b = unless b (fail "oof")
  n <- read <$> many1 digit
  unit <- string "cm" <|> string "in"
  case unit of
    "cm" -> failIfNot (within 150 193 n)
    "in" -> failIfNot (within 59 76 n)
    _ -> fail "bruh"

eyeCols = words "amb blu brn gry grn hzl oth"

within l h n = l <= n && n <= h
natWithin l h = do
  n <- nat
  if within l h n then pure n else fail "number out of range"

constrain l h n =
  if within l h n then pure n else fail "number out of range"

parsePass :: Parser Passport
parsePass = Passport <$> parseLf "byr" g
                     <*> optionMaybe (parseLf "cid" (many digit))
                     <*> parseLf "ecl" (foldr (\x y -> try x <|> y) (fail "invalid eye color") (string <$> eyeCols))
                     <*> parseLf "eyr" h
                     <*> parseLf "hcl" (char '#' *> replicateM 6 hexDigit)
                     <*> parseLf "hgt" hgtParse
                     <*> parseLf "iyr" gg
                     <*> parseLf "pid" (replicateM 9 digit)
  where
    g = do
      n <- read <$> replicateM 4 digit
      constrain 1920 2002 n
    h = do
      n <- read <$> replicateM 4 digit
      constrain 2020 2030 n
    gg = do
      n <- read <$> replicateM 4 digit
      constrain 2010 2020 n

gaga = parse parsePass ""
validPassPassed s = isRight (parse parsePass "" s)
data Passport = Passport
  {
    byr :: Int,
    cid :: Maybe String,
    ecl :: EyeCol,
    eyr :: Int,
    hcl :: String,
    hgt :: (),
    iyr :: Int,
    pid :: PassNo
  } deriving Show

-- check2 l = l'
--   where
--     l' = (\[a,b] -> (a,b)) <$> splitBy ':' l

--     -- allLabels = ["byr","cid","ecl","eyr","hcl","hgt","iyr","pid"]
--     -- isValid ls = allLabels == sort ls || (allLabels \\ ["cid"]) == sort ls

exValid = sortBy (compare `on` fst) ((\[a,b] -> (a,b)) . splitBy ':' <$> s)
  where
    s = words "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f"


exValidStr = unwords (sort (words "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f"))

exInvalidStr = unwords (sort (words  "hcl:dab227 iyr:2012 ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277" ))
main = do
  let dayNumber = 4 :: Int -- FIXME: change day number
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  let inp' = (++ " ") <$> (unwords <$> (sort <$> (words <$> (unwords <$> splitBy "" inp))))
  print (length (filter validPassPassed inp'))

--   -- print (takeWhile (/= "")inp)
--   let inp' = check <$> (words <$> (unwords <$> splitBy "" inp))
--   -- print (takeWhile (/= "\n") inp')
--   print (length (filter id inp'))
--   -- print (part1 inp)
--   -- print (part2 inp)
--   -- defaultMain [ bgroup dayString [ bench "part1" $ whnf part1 inp
--   --                             , bench "part2" $ whnf part2 inp
--   --                             ] ]

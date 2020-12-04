{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad
import Criterion.Main
import Data.Either
import Data.Functor
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Parsec
import qualified Text.Parsec.Text as PT
import qualified Text.Parsec.ByteString as BT

tok :: PT.Parser a -> PT.Parser a
tok = (<* space)

-- parse labelled field
parseLf l p = do
  string l
  string ":"
  tok p

nat :: PT.Parser Int
nat = read <$> many1 digit

hgtParse = do
  let failIfNot b = unless b (fail "oof")
  n <- nat
  unit <- string "cm" <|> string "in"
  case unit of
    "cm" -> failIfNot (within 150 193 n)
    "in" -> failIfNot (within 59 76 n)
    _ -> fail "bruh"

within l h n = l <= n && n <= h

data Passport = Passport
  { byr :: Int,
    cid :: Maybe (),
    ecl :: (),
    eyr :: Int,
    hcl :: T.Text,
    hgt :: (),
    iyr :: Int,
    pid :: ()
  }
  deriving (Show)

hclParse = char '#' *> (T.pack <$> replicateM 6 hexDigit)

eclParse = foldr (\x y -> try x <|> y) (fail "invalid eye color") (string <$> eyeCols) $> ()
  where
    eyeCols = words "amb blu brn gry grn hzl oth"

parsePass :: PT.Parser Passport
parsePass =
  Passport <$> parseLf "byr" g
    <*> optionMaybe (parseLf "cid" nat $> ())
    <*> parseLf "ecl" eclParse
    <*> parseLf "eyr" h
    <*> parseLf "hcl" hclParse
    <*> parseLf "hgt" hgtParse
    <*> parseLf "iyr" gg
    <*> parseLf "pid" (replicateM_ 9 digit)
  where
    nat4Within l h = do
      n <- read <$> replicateM 4 digit
      if within l h n then pure n else fail "number out of range"
    g = nat4Within 1920 2002
    h = nat4Within 2020 2030
    gg = nat4Within 2010 2020

validPassPassed s = isRight (parse parsePass "" s)

part1 = length . filter check

part2 = length . filter validPassPassed

check l = isValid l'
  where
    l' = T.takeWhile (/= ':') <$> l
    allLabels = sort ["eyr", "iyr", "byr", "ecl", "pid", "hcl", "hgt", "cid"]
    isValid ls = allLabels == sort ls || (allLabels \\ ["cid"]) == sort ls

main = do
  let dayNumber = 4 :: Int -- FIXME: change day number
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- TIO.readFile dayFilename
  let inp' = T.words . T.unwords . T.lines <$> T.splitOn "\n\n" inp
  let inp'' = (<> " ") . T.unwords . sort <$> inp'
  print (part1 inp')
  print (part2 inp'')
  defaultMain [ bgroup dayString [ bench "part1" $ whnf part1 inp'
                                 , bench "part2" $ whnf part2 inp''
                                 ] ]

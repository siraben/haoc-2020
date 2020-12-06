{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Criterion.Main
import qualified Data.ByteString.Char8 as B
import Data.Either
import Data.Functor
import Data.List
import Text.Parsec
import qualified Text.Parsec.ByteString as BT

-- parse labelled field
{-# INLINE parseLf #-}
parseLf :: String -> BT.Parser a -> BT.Parser ()
parseLf l p = string l *> char ':' *> (p <* space) $> ()

nat :: BT.Parser Int
nat = read <$> many1 digit

hgtParse = do
  n <- nat
  (string "cm" *> guard (within 150 193 n)) <|> (string "in" *> guard (within 59 76 n))

within :: Int -> Int -> Int -> Bool
within l h n = l <= n && n <= h

hclParse :: BT.Parser ()
hclParse = char '#' *> count 6 hexDigit $> ()

eclParse = do
  n <- count 3 lower
  guard (f n)
  where
    f "amb" = True
    f "blu" = True
    f "brn" = True
    f "gry" = True
    f "grn" = True
    f "hzl" = True
    f "oth" = True
    f _ = False

-- Parse a valid passport
parsePass :: BT.Parser ()
parsePass = do
  parseLf "byr" (nat4Within "1920" "2002")
  optionMaybe (parseLf "cid" (many1 digit))
  parseLf "ecl" eclParse
  parseLf "eyr" (nat4Within "2020" "2030")
  parseLf "hcl" hclParse
  parseLf "hgt" hgtParse
  parseLf "iyr" (nat4Within "2010" "2020")
  parseLf "pid" (count 9 digit)
  where
    nat4Within :: String -> String -> BT.Parser ()
    nat4Within l h = do
      n <- count 4 digit
      if l <= n && n <= h then pure () else fail "number out of range"

validPassport1 :: [B.ByteString] -> Bool
validPassport1 (_ : _ : _ : _ : _ : _ : _ : _ : _) = True
validPassport1 l@(_ : _ : _ : _ : _ : _ : _ : _) = not (any ("cid" `B.isPrefixOf`) l)
validPassport1 _ = False

validPassport2 = isRight . parse parsePass ""

part1 :: [[B.ByteString]] -> Int
part1 = length . filter validPassport1

part2 :: [B.ByteString] -> Int
part2 = length . filter validPassport2

splitOn' del bs = go bs
  where
    n = B.length del
    go bs = case B.breakSubstring del bs of
              (ls, rest) ->
                if B.null rest
                  then ls : mempty
                  else ls : splitOn' del (B.drop n rest)

main = do
  let dayNumber = 4 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- B.readFile dayFilename
  let inp' = sort . B.words . B.unwords . B.lines <$> splitOn' "\n\n" inp
  let inp'' = (<> " ") . B.unwords <$> inp'
  print (part1 inp')
  print (part2 inp'')
  defaultMain [ bgroup dayString [ bench "part1" $ whnf part1 inp'
                                 , bench "part2" $ whnf part2 inp''
                                 ] ]

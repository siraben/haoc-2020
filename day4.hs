{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Criterion.Main
import Data.Either
import Data.Functor
import Data.List
import Text.Parsec
import qualified Text.Parsec.ByteString as BT
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as B

tok :: BT.Parser a -> BT.Parser a
tok = (<* space)

-- parse labelled field
parseLf l p = string l *> char ':' *> tok p

nat :: BT.Parser Int
nat = read <$> many1 digit

hgtParse = do
  n <- nat
  (string "cm" *> guard (within 150 193 n)) <|> (string "in" *> guard (within 59 76 n))

within :: Int -> Int -> Int -> Bool
within l h n = l <= n && n <= h

data Passport = Passport
  { byr :: (),
    cid :: Maybe (),
    ecl :: (),
    eyr :: (),
    hcl :: (),
    hgt :: (),
    iyr :: (),
    pid :: ()
  }
  deriving (Show)

hclParse = char '#' *> count 6 hexDigit $> ()

eclParse = do
  n <- count 3 lower
  guard (n `S.member` eyeCols)
  where
    eyeCols = S.fromList (words "amb blu brn gry grn hzl oth")

-- Parse a valid passport
parsePass :: BT.Parser Passport
parsePass =
  Passport <$> parseLf "byr" (nat4Within "1920" "2002")
    <*> optionMaybe (parseLf "cid" nat $> ())
    <*> parseLf "ecl" eclParse
    <*> parseLf "eyr" (nat4Within "2020" "2030")
    <*> parseLf "hcl" hclParse
    <*> parseLf "hgt" hgtParse
    <*> parseLf "iyr" (nat4Within "2010" "2020")
    <*> parseLf "pid" (count 9 digit $> ())
  where
    nat4Within :: String -> String -> BT.Parser ()
    nat4Within l h = do
      n <- count 4 digit
      if l <= n && n <= h then pure () else fail "number out of range"

validPassPassed = isRight . parse parsePass ""

part1 = length . filter check

part2 = length . filter validPassPassed

check :: [B.ByteString] -> Bool
check (_:_:_:_:_:_:_:_:_) = True
check l@(_:_:_:_:_:_:_:_) = not (any ("cid" `B.isPrefixOf`) l)
check _ = False

splitOn' :: B.ByteString -> B.ByteString -> [B.ByteString]
splitOn' del bs | B.null bs = []
splitOn' del bs =
  case B.breakSubstring del bs of
    (ls, rest) ->
      if B.null rest
        then ls : mempty
        else ls : splitOn' del (B.tail rest)

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

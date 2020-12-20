{-# LANGUAGE LambdaCase #-}

import Criterion.Main
import Data.Foldable
import Data.Functor
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List
import Data.Maybe
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.ReadP as P

-- | Count the number of items in a container where the predicate is true.
countTrue :: Foldable f => (a -> Bool) -> f a -> Int
countTrue p = length . filter p . toList

data RuleD = Seq [Int] | Alt [Int] [Int] | Lit String
  deriving (Show, Eq)

type Rule = (Int, RuleD)

tok :: Parser a -> Parser a
tok p = p <* spaces

symb = tok . string

num :: Parser Int
num = tok (read <$> many1 digit)

simple = do
  char '"'
  s <- manyTill anyChar (try (char '"'))
  pure (Lit s)

parseRule :: Parser Rule
parseRule = do
  d <- num
  symb ":"
  b <- try alt <|> try compound <|> simple
  pure (d, b)
  where
    alt = do
      l <- many1 num
      symb "|"
      r <- many1 num
      pure (Alt l r)
    compound = Seq <$> many1 num

pp = parse parseRule ""

type RuleP = (Int, Parser ())

type Env = IntMap RuleD

foo :: (Int, RuleD) -> Env -> RuleP
foo (i, b) e = case b of
  Lit s -> (i, string s $> ())
  Alt l r ->
    ( i,
      -- translate to parser combinators
      try (foldl1' (*>) [p | u <- [(i, e IM.! i) | i <- l], let (_, p) = foo u e])
        <|> foldl1' (*>) [p | u <- [(i, e IM.! i) | i <- r], let (_, p) = foo u e]
    )
  Seq l -> (i, foldl1' (*>) [p | u <- [(i, e IM.! i) | i <- l], let (_, p) = foo u e])

parseMessage :: Env -> Int -> String -> Maybe [String]
parseMessage rules ruleIx input = toMaybe $ P.readP_to_S parser input
  where
    parser = parserFor ruleIx <* P.eof
    toMaybe = \case
      [(s, "")] -> Just s
      _ -> Nothing
    -- parserFor :: Int -> P.ReadP String
    parserFor ix = go (rules IM.! ix)
      where
        go = \case
          Lit s -> return <$> P.string s
          Alt l r -> asum (ruleList <$> [l, r])
          Seq l -> ruleList l
        ruleList rs = concat <$> traverse parserFor rs

part1 (ruleMap, strs) = countTrue isJust (parseMessage ruleMap 0 <$> strs)

change =
  IM.insert 8 (Alt [42] [42, 8])
    . IM.insert 11 (Alt [42, 31] [42, 11, 31])

part2 = part1

main = do
  let dayNumber = 19
      dayString = "day" <> show dayNumber
      dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  let (ps, strs) = span (elem ':') inp
      Right rules = traverse pp ps
      ruleMap = IM.fromList rules :: Env
  let ruleMap' = change ruleMap
  print (part1 (ruleMap, strs))
  print (part2 (ruleMap', strs))
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 (ruleMap, strs),
          bench "part2" $ whnf part2 (ruleMap', strs)
        ]
    ]

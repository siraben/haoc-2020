import Data.Either
import Data.Foldable
import Data.Functor
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List
import Text.ParserCombinators.Parsec

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

main = do
  let dayNumber = 19
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  let (ps, inps') = span (elem ':') inp
  let inps = tail inps'
  let Right inp' = traverse pp ps
  print inp'
  let inp'' = IM.fromList inp' :: Env
  -- print (countTrue isRight ((parse ((snd (foo (0, inp'' IM.! 0) inp'')) <* eof) "") <$> inps))
  let rule0 = snd (foo (0, inp'' IM.! 0) inp'') <* eof
  -- print (parse rule0 "" "bbbbbbbaaaabbbbaaabbabaaa")
  -- mapM_ print (filter (isRight . parse rule0 "") inps)
  print (countTrue isRight (parse rule0 "" <$> inps))

-- print (part1 inp)
-- print (part2 inp)
-- defaultMain
--   [ bgroup
--       dayString
--       [ bench "part1" $ whnf part1 inp,
--         bench "part2" $ whnf part2 inp
--       ]
--   ]

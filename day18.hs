import Text.ParserCombinators.Parsec
import Data.Functor
import Criterion.Main

part1 inp = sum <$> traverse peval inp
part2 inp = sum <$> traverse peval2 inp

parens :: Parser a -> Parser a
parens p = string "(" *> p <* string ")"
tok :: Parser a -> Parser a
tok p = p <* spaces
symb = tok . string
expr = term `chainl1` addop
term = tok (num <|> parens expr)
num = read <$> many1 digit
addop = (symb "+" $> (+)) <|> (symb "*" $> (*))
peval = parse expr ""

-- mul lower precedence than add
expr2 = term2 `chainl1` (symb "*" $> (*))
term2 = factor2 `chainl1` (symb "+" $> (+))
factor2 = tok (num <|> parens expr2)
peval2 = parse expr2 ""

main = do
  let dayNumber = 18
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  print (part1 inp)
  print (part2 inp)
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp,
          bench "part2" $ whnf part2 inp
        ]
    ]

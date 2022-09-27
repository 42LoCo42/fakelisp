module Main where

import Data.Either (fromRight)
import Text.Parsec
import Text.Printf (printf)

type Parser p = Parsec String () p

data Space = Normal | Special deriving (Eq, Show)

type Bracket = (Char, Char)

brRound, brSquare, brCurly, brAngle :: Bracket
brRound = ('(', ')')
brSquare = ('[', ']')
brCurly = ('{', '}')
brAngle = ('<', '>')

brackets :: [Bracket]
brackets = [brRound, brSquare, brCurly, brAngle]

data Value
  = Int Int
  | List Bracket [Value]

instance Show Value where
  show (Int i) = show i
  show (List b l) =
    printf
      "%c%s%c"
      (fst b)
      (unwords $ map show l)
      (snd b)

commentP :: Parser String
commentP = char '#' *> (dropWhile (== ' ') <$> many (noneOf "\n"))

emptyP :: Parser ()
emptyP = skipMany (space <|> ' ' <$ commentP)

spaceP :: Parser Space
spaceP = do
  let normal = " \t"
  let special = "\n,"
  val <- many1 (oneOf (normal ++ special) <|> '\n' <$ commentP)
  if any (`elem` val) special
    then return Special
    else return Normal

intP :: Parser Value
intP = Int . read <$> many1 digit

listP :: Parser Value
listP = do
  opening <- oneOf (map fst brackets) <* optional spaceP
  let btup = head $ filter (\t -> fst t == opening) brackets
  List btup . reverse <$> innerListP btup False []

innerListP :: Bracket -> Bool -> [Value] -> Parser [Value]
innerListP outerBracket multiMode acc0 = do
  value <- valueP
  let acc = value : acc0
  let flush mlm' =
        if mlm' && length acc > 1
          then [List outerBracket $ reverse acc]
          else acc
  maybeSpc <- optionMaybe spaceP
  maybeEnd <- optionMaybe (eof <|> () <$ char (snd outerBracket))

  case (maybeSpc, maybeEnd) of
    (_, Just _) -> return $ flush multiMode
    (Just Normal, _) -> innerListP outerBracket multiMode acc
    (Just Special, _) -> (++ flush True) <$> innerListP outerBracket True []
    _ -> error "Impossible" -- either space or end will always appear

valueP :: Parser Value
valueP =
  foldl1
    (<|>)
    [ intP,
      listP
    ]

fileP :: Parser [Value]
fileP = emptyP *> valueP `sepEndBy` emptyP

main :: IO ()
main = do
  let test = "test.fl"
  file <- readFile test
  mapM_ print $ fromRight [] $ parse fileP test file

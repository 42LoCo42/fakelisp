module Main where

import Control.Monad (replicateM)
import Data.Char (isAsciiLower, isAsciiUpper)
import Data.Maybe (isJust)
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
brackets = [brRound, brSquare, brCurly]

data Value
  = Int Int
  | Atom String
  | Char Char
  | String String
  | List Bracket [Value]

instance Show Value where
  show (Int i) = show i
  show (Atom a) = a
  show (Char c) = printf "'%c'" c
  show (String s) = printf "\"%s\"" s
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

atomP :: Parser Value
atomP = Atom <$> many1 (satisfy (\c -> any (\f -> f c) [isAsciiLower, isAsciiUpper, (`elem` symbols)]))
  where
    symbols = "!?+-*/<=>$%&~:@"

rawCharP :: Parser Char
rawCharP = do
  start <- noneOf "\""
  case start of
    '\\' -> do
      code <- anyChar
      case code of
        'n' -> return '\n'
        't' -> return '\t'
        'u' -> do
          digits <- replicateM 4 hexDigit
          return $ read $ printf "'\\x%s'" digits
        c -> return c
    c -> return c

charP :: Parser Value
charP = char '\'' *> (Char <$> rawCharP) <* char '\''

stringP :: Parser Value
stringP = char '"' *> (String <$> many rawCharP) <* char '"'

listP :: Parser Value
listP = do
  opening <- oneOf (map fst brackets) <* optional spaceP
  let btup = head $ filter (\t -> fst t == opening) brackets
  -- handle empty lists
  end <- optional spaceP *> optionMaybe (char $ snd btup)
  if isJust end
    then return $ List btup []
    else List btup . reverse <$> innerListP btup False []

defaultInnerListP :: Bracket -> Parser [Value]
defaultInnerListP b = reverse <$> innerListP b False []

innerListP :: Bracket -> Bool -> [Value] -> Parser [Value]
innerListP outerBracket multiMode acc0 = do
  value <- optional spaceP *> valueP
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
      atomP,
      charP,
      stringP,
      listP
    ]

fileP :: Parser [Value]
fileP = defaultInnerListP brRound

main :: IO ()
main = do
  let test = "test.fl"
  file <- readFile test
  let vals = parse fileP test file
  either print (mapM_ print) vals

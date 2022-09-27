module Parser where

import Control.Monad (replicateM)
import Data.Char (isDigit, isLetter)
import Data.Maybe (isJust)
import Text.Parsec
import Text.Printf (printf)
import Types

commentP :: Parser String
commentP = char '#' *> (dropWhile (== ' ') <$> many (noneOf "\n"))

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
atomP = Atom <$> many1 (satisfy (\c -> any (\f -> f c) options))
  where
    symbols = "!?+-*/=%&~:@.|"
    options =
      [ isLetter,
        isDigit,
        (`elem` symbols)
      ]

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

rawStringP :: Parser String
rawStringP = many rawCharP

stringP :: Parser Value
stringP = char '"' *> (String <$> rawStringP) <* char '"'

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
  maybeSpc <- (Just Special ==) <$> optionMaybe spaceP
  maybeEnd <- optionMaybe (eof <|> () <$ char (snd outerBracket))
  case (maybeSpc, maybeEnd) of
    (_, Just _) -> return $ flush multiMode
    (False, _) -> innerListP outerBracket multiMode acc
    (True, _) -> (++ flush True) <$> innerListP outerBracket True []

valueP :: Parser Value
valueP =
  foldl1
    (<|>)
    [ listP,
      intP,
      atomP,
      charP,
      stringP
    ]

fileP :: Parser [Value]
fileP = defaultInnerListP brRound

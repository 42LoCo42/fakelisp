{-# LANGUAGE MultiWayIf #-}
module Parser where

import Control.Monad (replicateM, void)
import Data.Char     (isDigit, isLetter)
import Data.Functor  (($>))
import Data.List     (find)
import Data.Maybe    (fromJust, isNothing)
import Text.Parsec
import Text.Printf   (printf)
import Types

commentP :: Parser ()
commentP = char '#' *> many (noneOf "\n") $> ()

sepP :: Bool -> Parser Sep
sepP allowComma = do
  chars <- many (commentP $> '\n' <|> oneOf " \t,;\n")
  let commaCount = if allowComma then length $ filter (`elem` ",;") chars else 0
  let breakCount = length $ filter (== '\n') chars
  if
    | commaCount >  1 -> fail "Only one comma allowed per separator"
    | commaCount == 1 -> return Comma
    | breakCount >= 1 -> return Break
    | otherwise       -> return Normal

intP :: Parser Value
intP = Int . read <$> many1 digit

atomP :: Parser Value
atomP = Atom <$> many1 (satisfy (\c -> any (\f -> f c) options))
  where
    symbols = "+-*/<=>^!$%&?`~:.@|"
    options =
      [ isLetter
      , isDigit
      , (`elem` symbols)
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
  openChar <- oneOf $ map fst brackets
  let bracket = fromJust $ find ((== openChar) . fst) brackets
  blockP $ Just bracket

blockP :: Maybe Bracket -> Parser Value
blockP bracket = do
  let endP = optionMaybe $ maybe eof (void . char . snd) bracket
  let specialBreak = isNothing bracket || bracket == Just brCurly
  let item normal acc = do
        value <- valueP
        sep   <- sepP True
        let push    = value : acc
        let sublist = (False, [], ((List $ reverse push) :))
        let single  = (False, [], (value :))
        let (normal', acc', ret) = case (sep, bracket) of
              (Normal, _) -> (normal, push, id)
              (Comma,  _) ->
                if null acc then single else sublist
              (Break, _) ->
                if specialBreak || not (null acc)
                then sublist
                else single
        maybeEnd <- endP
        ret <$>
          if isNothing maybeEnd
          then item normal' acc'
          else return $ if
            | null acc'        -> []
            | length acc' == 1 -> if specialBreak then [List acc'] else acc'
            | normal'          -> reverse acc'
            | otherwise        -> [List $ reverse acc']
  void $ sepP False
  earlyEnd <- endP
  if isNothing earlyEnd
  then List <$> item True []
  else return $ List []

valueP :: Parser Value
valueP =
  foldl1
    (<|>)
    [ listP
    , intP
    , atomP
    , charP
    , stringP
    ]

fileP :: Parser Value
fileP = blockP Nothing

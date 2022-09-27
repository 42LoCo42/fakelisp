module Types where

import Text.Parsec (Parsec)
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

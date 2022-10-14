{-# LANGUAGE LambdaCase #-}
module Types where

import Text.Parsec (Parsec)
import Text.Printf (printf)

type Parser p = Parsec String () p

data Sep = Normal | Comma | Break deriving (Eq, Show)

type Bracket = (Char, Char)

brRound, brSquare, brCurly :: Bracket
brRound  = ('(', ')')
brSquare = ('[', ']')
brCurly  = ('{', '}')

brackets :: [Bracket]
brackets = [brRound, brSquare, brCurly]

data Value
  = Int Int
  | Atom String
  | Char Char
  | String String
  | List [Value]

instance Show Value where
  show (Int i)    = show i
  show (Atom a)   = a
  show (Char c)   = printf "'%c'" c
  show (String s) = printf "\"%s\"" s
  show (List l)   =
    if any (\case
               List _ -> True
               _      -> False
           ) l
    then printf "(\n%s)" $ concatMap (unlines . map ('\t' :) . lines . show) l
    else printf "(%s)" $ unwords $ map show l

data Toplevel
  = TLUse [String]
  | TLTypeDef
  | TLFuncDev

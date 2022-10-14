module Main where

import System.Environment (getArgs)
import Text.Parsec        (parse)

import Parser (fileP)

main :: IO ()
main = do
  args <- getArgs
  let test = if null args then "test.fl" else head args
  file <- readFile test
  let vals = parse fileP test file
  print vals

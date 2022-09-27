module Main where

import Parser
import System.Environment (getArgs)
import Text.Parsec

main :: IO ()
main = do
  args <- getArgs
  let test = if null args then "test.fl" else head args
  file <- readFile test
  let vals = parse fileP test file
  either print (mapM_ print) vals

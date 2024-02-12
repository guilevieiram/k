module Main where
import System.Environment (getArgs)

import Lexer
import Parser

preProcess :: String -> String
preProcess = map (replace '\n' ' ')
 where
  replace x y c = if c == x then y else c

pipe :: String -> Maybe (IO Token)
pipe source = do
  let termTokens = (get . lexer . preProcess) source
  let tokens = map Terminal termTokens
  Just (parsePipe tokens)
 where
  get (Just res) = res
  get Nothing = []

main :: IO ()
main = do
  args <- getArgs
  sourceCode <- readFile (head args)
  print sourceCode
  print $ (lexer . preProcess) sourceCode
  ast <- get (pipe sourceCode)
  print ast
  return ()
 where
  get Nothing = return ExprErr
  get (Just e) = e

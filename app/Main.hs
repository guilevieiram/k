module Main where

import System.Environment (getArgs)

import Lexer (tokenize)
import Parser (parse)
import Tokens 

preProcess :: String -> String
preProcess = map (replace '\n' ' ')
  where
    replace x y c = if c == x then y else c

pipe :: String -> IO Token
pipe source = do
    let termTokens = (get . tokenize . preProcess) source
    let tokens = map Terminal termTokens
    parse tokens
  where
    get (Just res) = res
    get Nothing = []

main :: IO ()
main = do
    args <- getArgs
    sourceCode <- readFile (head args)

    -- let tokenized = (tokenize . preProcess) sourceCode
    -- putStrLn $ "\nSource Code:\n\n" ++ sourceCode
    -- putStrLn $ "\nTokenized:\n\n" ++ show tokenized

    ast <- pipe sourceCode
    putStrLn $ "Parsed Code:\n\n" ++ show ast
    putStrLn "\n"
    return ()

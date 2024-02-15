module Main where

import System.Environment (getArgs)

import Interpreter
import Lexer
import Parser
import Semantic
import Tokens
import Types
import Utils

data CompileError
    = TokenizationError LexemeError
    | ParsingError ParserError
    | SemanticAnalysisError SemanticError Token
    | ExecutingError ExecutionError
    deriving (Show, Eq)

pipe :: String -> Either CompileError Token
pipe source = do
    tokens <- tokenize source ->> TokenizationError
    ast <- parse tokens ->> ParsingError
    _ <- analyse ast ->> \x -> SemanticAnalysisError x ast
    Right ast

execute :: Bool -> Either CompileError Token -> IO ()
execute _ (Left err) = putStrLn $ "Compilation error: " ++ show err
execute debug (Right ast) =
    if not debug
        then do
            _ <- interpret ast
            putStrLn ""
        else do
            putStrLn "\n\nProgram sucessfully compiled"
            putStrLn "------------------------------------------------------"
            putStrLn "\n\nAST for the program: "
            putStrLn "------------------------------------------------------"
            print ast
            putStrLn "------------------------------------------------------"
            putStrLn "\n\nExecuting the program:"
            putStrLn "------------------------------------------------------"
            execState <- interpret ast
            putStrLn "\n------------------------------------------------------"
            putStrLn "\n\nFinal Program State:"
            putStrLn "------------------------------------------------------"
            print execState
            putStrLn "------------------------------------------------------"


main :: IO ()
main = do
    let debug = False
    args <- getArgs
    sourceCode <- readFile (head args)
    execute debug . pipe $ sourceCode

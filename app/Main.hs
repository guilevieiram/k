module Main where

import System.Environment (getArgs)

import Interpreter
import Lexer
import Parser
import Semantic
import States
import Tokens
import Utils

data CompileError
    = TokenizationError LexemeError
    | ParsingError ParserError
    | SemanticAnalysisError SemanticError Token
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
            execState <- interpret ast
            case executionError execState of
                Nothing -> putStrLn ""
                Just err -> putStrLn $ "Error encountered during execution: \n" ++ show err
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

run :: Bool -> String -> IO ()
run debug = execute debug . pipe

main :: IO ()
main = do
    let debug = False
    args <- getArgs
    sourceCode <- readFile (head args)
    run debug sourceCode

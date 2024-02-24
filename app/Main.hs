module Main where

import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Interpreter
import Lexer
import Parser
import Semantic
import States
import Tokens
import Utils

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> processFile filename False
        [filename, "-d"] -> processFile filename True
        _ -> putStrLn "Usage: k filename [-d]" >> exitFailure

processFile :: FilePath -> Bool -> IO ()
processFile filename debugMode = do
    fileExists <- doesFileExist filename
    if fileExists
        then do
            fileContent <- readFile filename
            run fileContent debugMode
        else do
            putStrLn $ "File does not exist: " ++ filename
            exitFailure

run :: String -> Bool -> IO ()
run code debug = do
    putStrLn "Parsing program..."
    let ast = parseProgram code
    ast `seq` putStrLn "Finished Parsing!"
    putStrLn "Start execution...\n\n\n"
    putStrLn "------------------------------------------------------"
    executeAst debug ast

data CompileError
    = TokenizationError LexemeError
    | ParsingError ParserError
    | SemanticAnalysisError SemanticError Token
    deriving (Show, Eq)

parseProgram :: String -> Either CompileError Token
parseProgram source = do
    tokens <- tokenize source ->> TokenizationError
    ast <- parse tokens ->> ParsingError
    _ <- analyse ast ->> \x -> SemanticAnalysisError x ast
    Right ast

executeAst :: Bool -> Either CompileError Token -> IO ()
executeAst _ (Left err) = putStrLn $ "Compilation error: " ++ show err
executeAst debug (Right ast) =
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
            putStrLn "\n\nFinal Program Variables:"
            putStrLn "------------------------------------------------------"
            print $ variables execState
            putStrLn "------------------------------------------------------"

-- let tokens = tokenize sourceCode
-- print tokens
-- case tokens of
--     Left _ -> return ()
--     Right toks -> do
--         _ <- stepParse toks
--         return ()

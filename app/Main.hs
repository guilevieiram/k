module Main where

import System.Environment (getArgs)

import Lexer (LexemeError, tokenize)
import Parser
import Tokens
import Utils
import Semantic 

data CompileError
    = TokenizationError LexemeError
    | ParsingError ParserError
    | SemanticAnalysisError SemanticError Token
    deriving (Show, Eq)

pipe :: String -> Either CompileError (Token, Types)
pipe source = do
    tokens <- tokenize source ->> TokenizationError
    ast <- parse tokens ->> ParsingError
    programType <- analyse ast ->> \x -> SemanticAnalysisError x ast
    Right (ast, programType)

main :: IO ()
main = do
    args <- getArgs
    sourceCode <- readFile (head args)
    putStrLn "tokenizing source code"
    putStrLn sourceCode
    let tokenized = tokenize sourceCode
    print tokenized
    let p = pipe sourceCode
    case p of 
        Left err -> print err
        Right (ast, pType) -> do
            putStrLn $ "Parsed Code:\n\n" ++ show ast
            putStrLn "\n"
            putStrLn $ "Doing syntatic analysis:\n\n" ++ show pType
    

-- playground -> dont look too much
--
--
--
--
--
-- putStrLn $ "\nSource Code:\n\n" ++ sourceCode
-- putStrLn $ "\nTokenized:\n\n" ++ show tokenized

-- let toks = [Terminal (Type TInt), Var "y", Terminal EndStatement]
-- let stoks = zip toks [defaultState{line = i} | i <- [1 ..]]
-- putStrLn "result of main"
-- print stoks
-- let scan = parseScan stoks
-- putStr $ intercalate "\n" $ map show scan
--
-- putStrLn "\n"
--
-- print $ getBest scan
--
--
--
-- case tokenized of
--     Left _ -> return ()
--     Right tok -> do
--         let ttoks = Right [(Terminal token, state) | (token, state) <- tok]
--         putStrLn "on the file input\n\n"
--         putStrLn "1----------------------------"
--         print tokenized
--         best <- pTokenized ttoks
--         print best
--         putStrLn "2----------------------------"
--         best <- pTokenized best
--         print best
--         putStrLn "3----------------------------"
--         best <- pTokenized best
--         print best
--         putStrLn "4----------------------------"
--         best <- pTokenized best
--         print best

{- let mapTerminal val = [(Terminal token, state) | (token, state) <- val]
    putStrLn "one step of parsing"

    let stoks = zip toks [defaultState{line=i} | i <- [1..]]
    -- [ (Terminal Type TInt, defaultState{line=1})
    -- , (Var "x", defaultState{line=2})
    -- , (Terminal EndStatement)
    -- ]

    let lit = handleParser varParser
    -- let lit = mainParse

    print $ lit stoks

    -- case tokenized of
    --     Left _ -> return ()
    --     Right val -> print $ lit . mapTerminal $ drop 1 val
-}
{-
pTokenized :: Either ParserError [SToken] -> IO (Either ParserError [SToken])
pTokenized tok = case tok of
    Left _ -> return $ Right []
    Right t -> do
        let s = parseScan t
        putStr $ intercalate "\n" $ map show s
        let best = getBest s
        return best -}

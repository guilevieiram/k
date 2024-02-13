module Main where

import System.Environment (getArgs)

import Lexer (LexemeError, tokenize)
import Parser
import Tokens
import Utils

data CompileError
    = TokenizationError LexemeError
    | ParsingError ParserError
    deriving (Show, Eq)

pipe :: String -> Either CompileError Token
pipe source = do
    tokens <- tokenize source ->> TokenizationError
    ast <- parse tokens ->> ParsingError
    Right $ fst ast

main :: IO ()
main = do
    args <- getArgs
    sourceCode <- readFile (head args)
    let tokenized = tokenize sourceCode
    print tokenized
    let ast = pipe sourceCode
    putStrLn $ "Parsed Code:\n\n" ++ show ast
    putStrLn "\n"

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

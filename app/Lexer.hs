module Lexer where

import Control.Monad (msum)
import Data.Char (isSpace)
import Text.Regex.TDFA ((=~))

import Tokens

lexemes :: [(String, String -> TerminalToken)]
lexemes =
    [ ("\\(", const OpenParens)
    , ("\\)", const CloseParens)
    , ("\\{", const OpenBrace)
    , ("\\}", const CloseBrace)
    , ("\\[", const OpenBracket)
    , ("\\]", const CloseBracket)
    , (";", const EndStatement)
    , (":=", const Assigner)
    , ("\\+", (const . Operator) Plus)
    , ("\\-", (const . Operator) Minus)
    , ("\\*", (const . Operator) Times)
    , ("/", (const . Operator) Divide)
    , (">", (const . Operator) Gt)
    , ("<", (const . Operator) Lt)
    , (">=", (const . Operator) Ge)
    , ("<=", (const . Operator) Le)
    , ("=", (const . Operator) Eq)
    , ("/=", (const . Operator) Neq)
    , ("!", (const . Operator) Not)
    , ("return", const Return)
    , ("if", const If)
    , ("then", const Then)
    , ("else", const Else)
    , ("while", const While)
    , ("do", const Do)
    , ("int", (const . Type) TInt)
    , ("bool", (const . Type) TBool)
    , ("float", (const . Type) TFloat)
    , ("true", (const . BoolLiteral) True)
    , ("false", (const . BoolLiteral) False)
    , ("[0-9]+\\.[0-9]*", FloatLiteral . read)
    , ("[0-9]+", IntLiteral . read)
    , ("[a-zA-Z0-9]+", Identifier)
    ]

matchRegex :: String -> String -> Maybe (String, String)
matchRegex pattern input =
    let
        (_, m, after) = input =~ pattern :: (String, String, String)
     in
        if not (null m) then Just (m, after) else Nothing

match :: [Char] -> Maybe (TerminalToken, [Char])
match input = msum matches
  where
    regexToToken predicate token = do
        (m, a) <- matchRegex ('^' : predicate) input
        return (token m, a)
    matches = map (uncurry regexToToken) lexemes

tokenize :: String -> Maybe [TerminalToken]
tokenize input =
    if null trimmed
        then Just []
        else do
            (t, r) <- match trimmed
            next <- tokenize r
            return (t : next)
  where
    trimmed = dropWhile isSpace input

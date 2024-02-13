module Lexer where

import Control.Monad (msum)
import Data.Char (isSpace)
import Text.Regex.TDFA ((=~))

import Data.List (intercalate)
import Data.List.Split (splitOn)
import State
import Tokens

newtype LexemeError
    = UnidentifiedToken State -- when no regex match
    deriving (Show, Eq)

type SChar = (Char, State)
type SToken = (TerminalToken, State)

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

tagCoordinates :: String -> [SChar]
tagCoordinates string = intercalate [(' ', defaultState)] taggedLines
  where
    stringLines = splitOn "\n" string
    taggedLines =
        [ [ (char, defaultState{line = i, col = j})
          | (j, char) <- zip [1 ..] strLine
          ]
        | (i, strLine) <- zip [1 ..] stringLines
        ]

matchRegex :: String -> [SChar] -> Maybe ([SChar], [SChar])
matchRegex pattern input = if null m then Nothing else Just (mTagged, afterTagged)
  where
    (before, m, after) = map fst input =~ pattern :: (String, String, String)
    mAndAfterInput = drop (length before) input
    afterInput = drop (length before + length m) input
    mTagged = take (length m) mAndAfterInput
    afterTagged = take (length after) afterInput

match :: [SChar] -> Either LexemeError (SToken, [SChar])
match input =
    case matched of
        Nothing -> Left $ UnidentifiedToken ((snd . head) input)
        Just value -> Right value
  where
    regexToToken :: String -> (String -> TerminalToken) -> Maybe (SToken, [SChar])
    regexToToken predicate tokenizeLexeme = do
        (m, after) <- matchRegex ('^' : predicate) input
        let mString = map fst m :: String
        let mCoord = (snd . head) m
        let lexTok = (tokenizeLexeme mString, mCoord) :: SToken
        return (lexTok, after)
    matches = map (uncurry regexToToken) lexemes
    matched = msum matches

tokenizeTagged :: [SChar] -> Either LexemeError [SToken]
tokenizeTagged input =
    if null trimmed
        then return []
        else do
            (lexTok, after) <- match trimmed
            afterTok <- tokenizeTagged after
            return (lexTok : afterTok)
  where
    trimmed = dropWhile (\(char, _) -> isSpace char) input

tokenize :: [Char] -> Either LexemeError [SToken]
tokenize = tokenizeTagged . tagCoordinates

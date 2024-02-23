module Lexer where

import Control.Monad (msum)
import Data.Char (isSpace)
import Text.Regex.TDFA ((=~))

import Data.List (intercalate)
import Data.List.Split (splitOn)
import Tokens
import Utils

type SChar = (Char, Source)

newtype LexemeError
    = UnidentifiedToken Source -- when no regex match
    deriving (Show, Eq)

-- entrypoint
tokenize :: [Char] -> Either LexemeError [TerminalToken]
tokenize = tokenizeTagged . removeComments . tagCoordinates

lexemes :: [(String, String -> Source -> TerminalToken)]
lexemes =
    [ ("\\(", const OpenParens)
    , ("\\)", const CloseParens)
    , ("\\{", const OpenBrace)
    , ("\\}", const CloseBrace)
    , ("\\[", const OpenBracket)
    , ("\\]", const CloseBracket)
    , (";", const EndStatement)
    , (":=", const Assigner)
    , ("\\+", const PlusSign)
    , ("\\-", const MinusSign)
    , ("\\*", const Star)
    , ("&", const AndSign)
    , ("\\|", const OrSign)
    , ("/", const RightBar)
    , ("<=", const LeSign)
    , (">=", const GeSign)
    , ("<", const OpenChevron)
    , (">", const CloseChevron)
    , ("=", const EqSign)
    , ("/=", const NeqSign)
    , ("!", const NotSign)
    , (",", const Comma)
    , (":", const Collon)
    , ("return\\b", const Return)
    , ("if\\b", const If)
    , ("then\\b", const Then)
    , ("else\\b", const Else)
    , ("while\\b", const While)
    , ("do\\b", const Do)
    , ("int\\b", const IntType)
    , ("bool\\b", const BoolType)
    , ("float\\b", const FloatType)
    , ("str\\b", const StringType)
    , ("char\\b", const CharType)
    , ("nil\\b", const NilType)
    , ("array\\b", const ArrayType)
    , ("none\\b", \_ src -> NilLiteral src)
    , ("true\\b", \_ src -> BoolLiteral src True)
    , ("false\\b", \_ src -> BoolLiteral src False)
    , ("[0-9]+\\.[0-9]*", \s src -> FloatLiteral src $ read s)
    , ("\"([^\"]*)\"", \s src -> StringLiteral src (init . tail $ s))
    , ("\'([^\']*)\'", \s src -> CharLiteral src (init . tail $ s))
    , ("[0-9]+", \s src -> IntLiteral src $ read s)
    , ("[a-zA-Z0-9_]+", flip Identifier)
    ]

removeComments :: [SChar] -> [SChar]
removeComments (('/', _) : ('*', _) : rest) =
    removeComments . drop 2 $
        dropWhileList (not . matchEnd) rest
  where
    matchEnd (('*', _) : ('/', _) : _) = True
    matchEnd _ = False
removeComments (x : xs) = x : removeComments xs
removeComments [] = []

tagCoordinates :: String -> [SChar]
tagCoordinates string = intercalate [(' ', defaultSource)] taggedLines
  where
    stringLines = splitOn "\n" string
    taggedLines =
        [ [ (char, defaultSource{line = i, col = j})
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

match :: [SChar] -> Either LexemeError (TerminalToken, [SChar])
match input =
    case matched of
        Nothing -> Left $ UnidentifiedToken ((snd . head) input)
        Just value -> Right value
  where
    regexToToken :: String -> (String -> Source -> TerminalToken) -> Maybe (TerminalToken, [SChar])
    regexToToken predicate tokenizeLexeme = do
        (m, after) <- matchRegex ('^' : predicate) input
        let mString = map fst m :: String
        let mCoord = (snd . head) m
        let lexTok = tokenizeLexeme mString mCoord :: TerminalToken
        return (lexTok, after)
    matches = map (uncurry regexToToken) lexemes
    matched = msum matches

tokenizeTagged :: [SChar] -> Either LexemeError [TerminalToken]
tokenizeTagged input =
    if null trimmed
        then return []
        else do
            (lexTok, after) <- match trimmed
            afterTok <- tokenizeTagged after
            return (lexTok : afterTok)
  where
    trimmed = dropWhile (\(char, _) -> isSpace char) input

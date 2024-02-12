module Main where

import Control.Monad (msum)
import Data.Char (isSpace)
import Data.List
import Data.List.Split (splitOn)
import System.Environment (getArgs)
import Text.Regex.TDFA ((=~))

{- LEXER -}

data Types = Int | Float
  deriving (Show, Eq)

data Op
  = Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eq
  | Neq
  | Not
  deriving (Show, Eq)

data TerminalToken
  = ERR
  | OpenBrace
  | CloseBrace
  | OpenParens
  | CloseParens
  | EndStatement
  | Return
  | Assigner
  | Type Types
  | Operator Op
  | IntLiteral Int
  | Identifier [Char]
  deriving (Show, Eq)

reducers :: [(String, String -> TerminalToken)]
reducers =
  [ ("\\(", const OpenParens)
  , ("\\)", const CloseParens)
  , ("\\{", const OpenBrace)
  , ("\\}", const CloseBrace)
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
  , ("return", const Return)
  , ("int", (const . Type) Int)
  , ("float", (const . Type) Int)
  , ("[0-9]+", IntLiteral . read)
  , ("[a-zA-Z0-9]+", Identifier)
  ]

trim :: [Char] -> [Char]
trim = dropWhile isSpace

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
  matches = map (uncurry regexToToken) reducers

preProcess :: String -> String
preProcess = map (replace '\n' ' ')
 where
  replace x y c = if c == x then y else c

lexer :: String -> Maybe [TerminalToken]
lexer input =
  if null trimmed
    then Just []
    else do
      (t, r) <- match trimmed
      next <- lexer r
      return (t : next)
 where
  trimmed = trim input

{- PARSER -}
data Expr
  = Var String
  | Declare Types String
  | Assign String Expr
  | BinOp Op Expr Expr
  | IntLiteralExpr Int
  | Seq Expr Expr
  | Term TerminalToken
  | End
  deriving (Show, Eq)

splitOnTwo :: (Eq a) => a -> [a] -> ([a], [a])
splitOnTwo predicate list = (head s, intercalate [predicate] (tail s))
 where
  s = splitOn [predicate] list

parse :: [TerminalToken] -> Maybe Expr
parse tokens = case tokens of
  -- declaring a variable
  (Type t : Identifier x : EndStatement : rest) -> 
    if null rest then Just (Declare t x) else do
    parsed <- parse rest
    return $ Seq (Declare t x) parsed

  -- assigning an expr to a variable
  (Identifier x : Assigner : rest) -> do
    let (body, after) = splitOnTwo EndStatement rest
    parsedBody <- parse body
    parsedAfter <- parse after
    let assignment = Assign x parsedBody
    return $ if null after then assignment else Seq assignment parsedAfter

  -- Numeric Constants
  (IntLiteral i : rest) ->
    if null rest
      then Just (IntLiteralExpr i)
      else do
        parsed <- parse rest
        return $ Seq (IntLiteralExpr i) parsed

  _ -> Just End

pipeline :: String -> Maybe Expr
pipeline text = do
  tokens <- lexer . preProcess $ text
  parse tokens

main :: IO ()
main = do
  args <- getArgs
  sourceCode <- readFile (head args)
  print sourceCode
  print $ (lexer . preProcess) sourceCode
  let result = pipeline sourceCode
  print $ "result : " ++ show result
  -- print $ matchRegex "^[a-zA-Z]+" "value asdf 2"
  -- print $ matchRegex "^[0-9]." "123145 df"
  -- print $ matchRegex "^(int|float)" "int x = 1"
  print $ matchRegex "^(int|float)" "x; int y := 1"
  print $ matchRegex "^[a-zA-Z0-9]+" (trim " x;\nint y;")
  print $ match (trim " x ;\nint b")
  print $ match "x;\nint b"
  print $ lexer "int x; int b;"
  print $ splitOnTwo ';' "asdf;asdf;asdf;"
  -- print $ trim $ trim "x ;\nint b"
  --
  return ()

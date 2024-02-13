module Parser where

import Data.Either (isRight)
import State (State)
import Tokens

data PError
    = MissingEndStatement
    | ExtraEndStatement
    | NotMatched
    deriving (Show, Eq)

data ParserError = ParserError PError State
    deriving (Show, Eq)

type SToken = (Token, State)
type STerminalToken = (TerminalToken, State)

type Parser = [Token] -> Either PError ([Token], Token, [Token])

-- final parser
parsers :: [Parser]
parsers =
    [ literalParser
    , varParser
    , braceParser
    , opParser
    , sequenceParser
    , ifParser
    , whileParser
    , callParser
    , functionParser
    ]

-- manages the perror into a parser error with the state of the token
handleParser :: Parser -> [SToken] -> Either ParserError [SToken]
handleParser _ [] = Right []
handleParser parser sTokens = case parsed of
    Left err -> Left $ ParserError err representativeState
    Right (before, expr, after) -> do
        let sBefore = matchState before
        let sExpr = [(expr, representativeState)]
        let sAfter = matchStateRev after
        return $ sBefore ++ sExpr ++ sAfter
  where
    matchState toks = zip toks states
    matchStateRev toks = reverse $ zip (reverse toks) (reverse states)
    rawTokens = map fst sTokens
    states = map snd sTokens
    representativeState = snd $ head sTokens
    parsed = parser rawTokens

-- get the best parsed option from the given list
getBest :: [Either ParserError [SToken]] -> Either ParserError [SToken]
getBest [] = Right []
getBest expressions = head (rights ++ nonTrivialErrs ++ expressions)
  where
    catchNonTrivialErr (Left (ParserError NotMatched _)) = False
    catchNonTrivialErr (Left (ParserError _ _)) = True
    catchNonTrivialErr _ = False
    rights = filter isRight expressions
    nonTrivialErrs = filter catchNonTrivialErr expressions

-- composes the individual parsers
composedParser :: [SToken] -> [Either ParserError [SToken]]
composedParser tokens = [par tokens | par <- map handleParser parsers]

-- Parser entrypoint. calls the main
parseScan :: [SToken] -> [Either ParserError [SToken]]
parseScan expressions = parsedTentatives
  where
    prependT :: Int -> Either ParserError [SToken] -> Either ParserError [SToken]
    prependT i toks = case toks of
        Left err -> Left err
        Right val -> Right $ take i expressions ++ val
    parsePartialFrom i = map (prependT i) (composedParser (drop i expressions))
    parsedTentatives = concat $ zipWith (\i _ -> parsePartialFrom i) [0 ..] expressions

mainParser :: [SToken] -> Either ParserError SToken
mainParser [e] = Right e
mainParser expressions = getBest (parseScan expressions) >>= mainParser

-- preprocess terminal tokens that come from the lexer
parse :: [STerminalToken] -> Either ParserError SToken
parse terminalTokens = parsed
  where
    toks = [(Terminal token, state) | (token, state) <- terminalTokens]
    parsed = mainParser toks

-- Defining each individual parser
varParser :: Parser
varParser
    ( Terminal (Type _)
            : Var _
            : Terminal EndStatement
            : Terminal EndStatement
            : _
        ) = Left ExtraEndStatement
varParser
    ( Var x
            : Terminal Assigner
            : Expr e
            : Terminal EndStatement
            : rest
        ) = Right ([], Expr (Assign x e), rest)
varParser
    ( Var x
            : Terminal Assigner
            : Var y
            : Terminal EndStatement
            : rest
        ) = Right ([], Expr (Assign x (Var y)), rest)
varParser
    ( Terminal (Type t)
            : Var x
            : Terminal EndStatement
            : rest
        ) = Right ([], Expr (Declare t x), rest)
varParser
    ( Terminal (Identifier x)
            : rest
        ) = Right ([], Var x, rest)
varParser
    ( Terminal (Type _)
            : Var _
            : _
        ) = Left MissingEndStatement
varParser _ = Left NotMatched

braceParser :: Parser
braceParser
    ( Terminal OpenParens
            : Expr i
            : Terminal CloseParens
            : rest
        ) = Right ([], Expr i, rest)
braceParser
    ( Terminal OpenBrace
            : Expr i
            : Terminal CloseBrace
            : rest
        ) = Right ([], Expr (Sequence [i]), rest)
braceParser
    ( Terminal OpenBrace
            : Sequence s
            : Terminal CloseBrace
            : rest
        ) = Right ([], Expr (Sequence s), rest)
braceParser
    ( Terminal OpenBrace
            : Terminal CloseBrace
            : rest
        ) = Right ([], Expr (Sequence []), rest)
braceParser _ = Left NotMatched

literalParser :: Parser
literalParser (Terminal (IntLiteral i) : rest) = Right ([], Expr (IntValue i), rest)
literalParser (Terminal (FloatLiteral i) : rest) = Right ([], Expr (FloatValue i), rest)
literalParser (Terminal (BoolLiteral i) : rest) = Right ([], Expr (BoolValue i), rest)
literalParser _ = Left NotMatched

sequenceParser :: Parser
sequenceParser (Expr i : Expr j : rest) = Right ([], Sequence [i, j], rest)
sequenceParser (Sequence s : Expr i : rest) = Right ([], Sequence (s ++ [i]), rest)
sequenceParser (Expr j : Sequence s : rest) = Right ([], Sequence (j : s), rest)
sequenceParser _ = Left NotMatched

isUnary :: Op -> Bool
isUnary x = x `elem` [Not, Minus]

isBinary :: Op -> Bool
isBinary x = x /= Not

opParser :: Parser
opParser (Expr i : Terminal (Operator o) : Expr j : rest) =
    if isBinary o then Right ([], Expr (BinOp o i j), rest) else Left NotMatched
opParser (Var i : Terminal (Operator o) : Expr j : rest) =
    if isBinary o then Right ([], Expr (BinOp o (Var i) j), rest) else Left NotMatched
opParser (Var i : Terminal (Operator o) : Var j : rest) =
    if isBinary o then Right ([], Expr (BinOp o (Var i) (Var j)), rest) else Left NotMatched
opParser (Expr i : Terminal (Operator o) : Var j : rest) =
    if isBinary o then Right ([], Expr (BinOp o i (Var j)), rest) else Left NotMatched
opParser (Terminal (Operator o) : Expr i : rest) =
    if isUnary o then Right ([], Expr (UnOp o i), rest) else Left NotMatched
opParser (Terminal (Operator o) : Var i : rest) =
    if isUnary o then Right ([], Expr (UnOp o (Var i)), rest) else Left NotMatched
opParser _ = Left NotMatched

whileParser :: Parser
whileParser
    ( Terminal While
            : Expr b
            : Terminal Do
            : Expr e
            : rest
        ) = Right ([], Expr (WhileLoop b e), rest)
whileParser _ = Left NotMatched

ifParser :: Parser
ifParser
    ( Terminal If
            : Expr eif
            : Terminal Then
            : Expr ethen
            : Terminal Else
            : Expr eelse
            : rest
        ) = Right ([], Expr (Conditional eif ethen eelse), rest)
ifParser _ = Left NotMatched

functionParser :: Parser
functionParser
    ( Terminal OpenBracket
            : Terminal (Type t)
            : Var x
            : Terminal CloseBracket
            : rest
        ) = Right ([], Arg t x, rest)
functionParser (Arg ta a : rest) = Right ([], Args [Arg ta a], rest)
functionParser (Args s : Arg ta a : rest) = Right ([], Args (s ++ [Arg ta a]), rest)
functionParser
    ( Terminal (Type t)
            : Var x
            : Args s
            : Expr e
            : rest
        ) = Right ([], Expr (Function x t (Args s) e), rest)
functionParser
    ( Terminal (Type t)
            : Var x
            : Expr e
            : rest
        ) = Right ([], Expr (Function x t (Args []) e), rest)
functionParser
    ( Terminal Return
            : Expr e
            : Terminal EndStatement
            : rest
        ) = Right ([], Expr (ReturnVal e), rest)
functionParser
    ( Terminal Return
            : Var x
            : Terminal EndStatement
            : rest
        ) = Right ([], Expr (ReturnVal (Var x)), rest)
functionParser _ = Left NotMatched

callParser :: Parser
callParser
    ( Terminal OpenBracket
            : Terminal CloseBracket
            : rest
        ) = Right ([], CallArgs [], rest)
callParser
    ( Terminal OpenBracket
            : Expr e
            : Terminal CloseBracket
            : rest
        ) = Right ([], CallArg e, rest)
callParser
    ( Terminal OpenBracket
            : Var x
            : Terminal CloseBracket
            : rest
        ) = Right ([], CallArg (Var x), rest)
callParser (CallArg a : rest) = Right ([], CallArgs [a], rest)
callParser (CallArgs as : CallArg a : rest) = Right ([], CallArgs (as ++ [a]), rest)
callParser
    ( Var x
            : Terminal Assigner
            : Var f
            : CallArgs as
            : Terminal EndStatement
            : rest
        ) = Right ([], Expr (Assign x (Call f (CallArgs as))), rest)
callParser
    ( Var f
            : CallArgs as
            : Terminal EndStatement
            : rest
        ) = Right ([], Expr (Call f (CallArgs as)), rest)
callParser _ = Left NotMatched

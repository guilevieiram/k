module Parser where

import Data.Either (isRight)
import Tokens

type Parser = [Token] -> Either ParserError [Token]

data ParserError
    = MissingEndStatement Source
    | ExtraEndStatement Source
    | NotMatched Source
    deriving (Show, Eq)

-- entrypoint
parse :: [TerminalToken] -> Either ParserError Token
parse terminalTokens = mainParser $ map Terminal terminalTokens

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

getBest :: [Either ParserError [Token]] -> Either ParserError [Token]
getBest [] = Right []
getBest expressions = head (rights ++ nonTrivialErrs ++ expressions)
  where
    catchNonTrivialErr (Left (NotMatched _)) = False
    catchNonTrivialErr (Left _) = True
    catchNonTrivialErr _ = False
    rights = filter isRight expressions
    nonTrivialErrs = filter catchNonTrivialErr expressions

parseScan :: [Token] -> [Either ParserError [Token]]
parseScan tokens = parsedTentatives
  where
    composedParser toks = [par toks | par <- parsers]
    tryPrependTokens :: Int -> Either ParserError [Token] -> Either ParserError [Token]
    tryPrependTokens i toks = case toks of
        Left err -> Left err
        Right val -> Right $ take i tokens ++ val
    parsePartialFrom i = map (tryPrependTokens i) (composedParser (drop i tokens))
    parsedTentatives = concat $ zipWith (\i _ -> parsePartialFrom i) [0 ..] tokens

mainParser :: [Token] -> Either ParserError Token
mainParser [e] = Right e
mainParser expressions = getBest (parseScan expressions) >>= mainParser

-- Defining each individual parser
varParser :: Parser
varParser
    ( Terminal (Type src _)
            : Var _ _
            : Terminal (EndStatement _)
            : Terminal (EndStatement _)
            : _
        ) = Left $ ExtraEndStatement src
varParser
    ( Var src x
            : Terminal (Assigner _)
            : Expr _ e
            : Terminal (EndStatement _)
            : rest
        ) = Right $ Expr src (Assign src x e) : rest
varParser
    ( Var src x
            : Terminal (Assigner asrc)
            : Var srcy y
            : Terminal (EndStatement _)
            : rest
        ) = Right $ Expr src (Assign asrc x (Var srcy y)) : rest
varParser
    ( Terminal (Type src t)
            : Var _ x
            : Terminal (EndStatement _)
            : rest
        ) = Right $ Expr src (Declare src t x) : rest
varParser
    ( Terminal (Identifier src x)
            : rest
        ) = Right $ Var src x : rest
varParser
    ( Var src x
            : Terminal (EndStatement _)
            : rest
        ) = Right $ Expr src (Var src x) : rest
varParser
    ( Terminal (Type src _)
            : Var _ _
            : _
        ) = Left $ MissingEndStatement src
varParser _ = Left $ NotMatched defaultSource

braceParser :: Parser
braceParser
    ( Terminal (OpenParens _)
            : Expr src i
            : Terminal (CloseParens _)
            : rest
        ) = Right $ Expr src i : rest
braceParser
    ( Terminal (OpenBrace _)
            : Expr src i
            : Terminal (CloseBrace _)
            : rest
        ) = Right $ Expr src (Sequence src [i]) : rest
braceParser
    ( Terminal (OpenBrace _)
            : Sequence src s
            : Terminal (CloseBrace _)
            : rest
        ) = Right $ Expr src (Sequence src s) : rest
braceParser
    ( Terminal (OpenBrace src)
            : Terminal (CloseBrace _)
            : rest
        ) = Right $ Expr src (Sequence src []) : rest
braceParser _ = Left $ NotMatched defaultSource

literalParser :: Parser
literalParser (Terminal (IntLiteral src i) : rest) = Right $ Expr src (IntValue src i) : rest
literalParser (Terminal (FloatLiteral src i) : rest) = Right $ Expr src (FloatValue src i) : rest
literalParser (Terminal (BoolLiteral src i) : rest) = Right $ Expr src (BoolValue src i) : rest
literalParser _ = Left $ NotMatched defaultSource

sequenceParser :: Parser
sequenceParser (Expr src i : Expr _ j : rest) = Right $ Sequence src [i, j] : rest
sequenceParser (Sequence src s : Expr _ i : rest) = Right $ Sequence src (s ++ [i]) : rest
sequenceParser (Expr src j : Sequence _ s : rest) = Right $ Sequence src (j : s) : rest
sequenceParser _ = Left $ NotMatched defaultSource

isUnary :: Op -> Bool
isUnary x = x `elem` [Not, Minus]

isBinary :: Op -> Bool
isBinary x = x /= Not

opParser :: Parser
opParser (Expr _ i : Terminal (Operator src o) : Expr _ j : rest) =
    if isBinary o then Right $ Expr src (BinOp src o i j) : rest else Left $ NotMatched defaultSource
opParser (Var si i : Terminal (Operator src o) : Expr _ j : rest) =
    if isBinary o then Right $ Expr src (BinOp src o (Var si i) j) : rest else Left $ NotMatched defaultSource
opParser (Var si i : Terminal (Operator src o) : Var sj j : rest) =
    if isBinary o then Right $ Expr src (BinOp src o (Var si i) (Var sj j)) : rest else Left $ NotMatched defaultSource
opParser (Expr _ i : Terminal (Operator src o) : Var sj j : rest) =
    if isBinary o then Right $ Expr src (BinOp src o i (Var sj j)) : rest else Left $ NotMatched defaultSource
opParser (Terminal (Operator src o) : Expr _ i : rest) =
    if isUnary o then Right $ Expr src (UnOp src o i) : rest else Left $ NotMatched defaultSource
opParser (Terminal (Operator src o) : Var si i : rest) =
    if isUnary o then Right $ Expr src (UnOp src o (Var si i)) : rest else Left $ NotMatched defaultSource
opParser _ = Left $ NotMatched defaultSource

whileParser :: Parser
whileParser
    ( Terminal (While src)
            : Expr _ b
            : Terminal (Do _)
            : Expr _ e
            : rest
        ) = Right $ Expr src (WhileLoop src b e) : rest
whileParser _ = Left $ NotMatched defaultSource

ifParser :: Parser
ifParser
    ( Terminal (If src)
            : Expr _ eif
            : Terminal (Then _)
            : Expr _ ethen
            : Terminal (Else _)
            : Expr _ eelse
            : rest
        ) = Right $ Expr src (Conditional src eif ethen eelse) : rest
ifParser _ = Left $ NotMatched defaultSource

functionParser :: Parser
functionParser
    ( Terminal (OpenBracket src)
            : Terminal (Type _ t)
            : Var _ x
            : Terminal (CloseBracket _)
            : rest
        ) = Right $ Arg src t x : rest
functionParser (Arg src ta a : rest) = Right $ Args src [Arg src ta a] : rest
functionParser (Args src s : Arg sa ta a : rest) = Right $ Args src (s ++ [Arg sa ta a]) : rest
functionParser
    ( Terminal (Type src t)
            : Var _ x
            : Args sargs s
            : Expr _ e
            : rest
        ) = Right $ Expr src (Function src x t (Args sargs s) e) : rest
functionParser
    ( Terminal (Type src t)
            : Var _ x
            : Expr _ e
            : rest
        ) = Right $ Expr src (Function src x t (Args src []) e) : rest
functionParser
    ( Terminal (Return src)
            : Expr _ e
            : Terminal (EndStatement _)
            : rest
        ) = Right $ Expr src (ReturnVal src e) : rest
functionParser
    ( Terminal (Return src)
            : Var sx x
            : Terminal (EndStatement _)
            : rest
        ) = Right $ Expr src (ReturnVal src (Var sx x)) : rest
functionParser _ = Left $ NotMatched defaultSource

callParser :: Parser
callParser
    ( Terminal (OpenBracket src)
            : Terminal (CloseBracket _)
            : rest
        ) = Right $ CallArgs src [] : rest
callParser
    ( Terminal (OpenBracket src)
            : Expr _ e
            : Terminal (CloseBracket _)
            : rest
        ) = Right $ CallArg src e : rest
callParser
    ( Terminal (OpenBracket src)
            : Var sx x
            : Terminal (CloseBracket _)
            : rest
        ) = Right $ CallArg src (Var sx x) : rest
callParser (CallArg src a : rest) = Right $ CallArgs src [a] : rest
callParser (CallArgs src as : CallArg _ a : rest) = Right $ CallArgs src (as ++ [a]) : rest
callParser
    ( Var src x
            : Terminal (Assigner sass)
            : Var sf f
            : CallArgs sas as
            : Terminal (EndStatement _)
            : rest
        ) = Right $ Expr src (Assign sass x (Call sf f (CallArgs sas as))) : rest
callParser
    ( Var sf f
            : CallArgs sas as
            : Terminal (EndStatement _)
            : rest
        ) = Right $ Expr sf (Call sf f (CallArgs sas as)) : rest
callParser _ = Left $ NotMatched defaultSource

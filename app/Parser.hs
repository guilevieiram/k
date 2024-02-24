module Parser where

import Data.Either (isRight)

import Tokens
import Utils (processEscapes)

type Parser = [Token] -> Either ParserError [Token]

data ParserError
    = MissingEndStatement Source
    | ExtraEndStatement Source
    | CharTooLong Source
    | EmptyChar Source
    | NotMatched Source
    deriving (Show, Eq)

-- entrypoint
parse :: [TerminalToken] -> Either ParserError Token
parse terminalTokens = mainParser $ map Terminal terminalTokens

-- for debugging
stepParse :: [TerminalToken] -> IO Token
stepParse terms = mainStepParser $ map Terminal terms
mainStepParser :: [Token] -> IO Token
mainStepParser [e] = return e
mainStepParser expressions = do
    print best
    mainStepParser iobest
  where
    best = getBest (parseScan expressions)
    iobest = case best of
        Left _ -> [Terminal (EndStatement defaultSource)]
        Right b -> b

parsers :: [Parser]
parsers =
    [ typeParser
    , literalParser
    , varParser
    , arrayParser
    , argsParser
    , opParser
    , ifParser
    , functionParser
    , whileParser
    , braceParser
    , callParser
    , sequenceParser
    , stmtParser
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
parseScan tokens = concatMap tryParseAll parsers
  where
    idxs = zipWith const [0 ..] tokens
    tryParse parser i = (take i tokens ++) <$> parser (drop i tokens)
    tryParseAll parser = map (tryParse parser) idxs

mainParser :: [Token] -> Either ParserError Token
mainParser [e] = Right e
mainParser expressions = getBest (parseScan expressions) >>= mainParser

-- types
typeParser :: Parser
typeParser (Terminal (IntType src) : rest) =
    Right $ TypeDeclaration src TInt : rest
typeParser (Terminal (FloatType src) : rest) =
    Right $ TypeDeclaration src TFloat : rest
typeParser (Terminal (BoolType src) : rest) =
    Right $ TypeDeclaration src TBool : rest
typeParser (Terminal (StringType src) : rest) =
    Right $ TypeDeclaration src TString : rest
typeParser (Terminal (CharType src) : rest) =
    Right $ TypeDeclaration src TChar : rest
typeParser (Terminal (NilType src) : rest) =
    Right $ TypeDeclaration src TNil : rest
typeParser
    ( Terminal (ArrayType src)
            : Terminal (OpenChevron _)
            : TypeDeclaration _ inner
            : Terminal (CloseChevron _)
            : rest
        ) = Right $ TypeDeclaration src (TArray inner) : rest
typeParser _ = Left $ NotMatched defaultSource

-- Defining each individual parser
varParser :: Parser
varParser
    ( td@(TypeDeclaration _ _)
            : Terminal (Identifier src f)
            : bracket@(Terminal (OpenParens _))
            : rest
        ) = Right $ td : FunctionName src f : bracket : rest
varParser
    ( Terminal (Identifier src f)
            : bracket@(Terminal (OpenParens _))
            : rest
        ) = Right $ FunctionName src f : bracket : rest
varParser
    ( Terminal (Identifier src x)
            : ass@(Terminal (Assigner _))
            : rest
        ) = Right $ Var src x [] : ass : rest
varParser
    ( td@(TypeDeclaration _ _)
            : Terminal (Identifier src x)
            : rest
        ) = Right $ td : Var src x [] : rest
varParser
    ( Terminal (Identifier src x)
            : rest
        ) = Right $ Expr src (Var src x []) : rest
varParser
    ( TypeDeclaration src t
            : var@(Var _ x _)
            : Terminal (Assigner _)
            : Expr _ e
            : Terminal (EndStatement _)
            : rest
        ) = Right $ Stmt src (Declare src t x) : Stmt src (Assign src var e) : rest
varParser
    ( var@(Var src _ _)
            : Terminal (Assigner _)
            : Expr _ e
            : Terminal (EndStatement _)
            : rest
        ) = Right $ Stmt src (Assign src var e) : rest
varParser
    ( TypeDeclaration src t
            : Var _ x _
            : Terminal (EndStatement _)
            : rest
        ) = Right $ Stmt src (Declare src t x) : rest
varParser _ = Left $ NotMatched defaultSource

-- arrays get and set
arrayParser :: Parser
arrayParser
    ( Expr _ (Var _ name as)
            : Terminal (OpenBracket src)
            : e@(Expr _ _)
            : Terminal (CloseBracket _)
            : t@(Terminal (Assigner _))
            : rest
        ) = Right $ Var src name (as ++ [e]) : t : rest
arrayParser
    ( Expr _ (Var _ name as)
            : Terminal (OpenBracket src)
            : e@(Expr _ _)
            : Terminal (CloseBracket _)
            : rest
        ) = Right $ Expr src (Var src name (as ++ [e])) : rest
arrayParser _ = Left $ NotMatched defaultSource

-- braces
braceParser :: Parser
braceParser
    ( Terminal (OpenParens _)
            : Expr src i
            : Terminal (CloseParens _)
            : rest
        ) = Right $ Expr src i : rest
braceParser
    ( Terminal (OpenBrace _)
            : s@(Stmt _ _)
            : Terminal (CloseBrace _)
            : rest
        ) = Right $ s : rest
braceParser
    ( Terminal (OpenBrace _)
            : Sequence src s
            : Terminal (CloseBrace _)
            : rest
        ) = Right $ Stmt src (Sequence src s) : rest
braceParser
    ( Terminal (OpenBrace src)
            : Terminal (CloseBrace _)
            : rest
        ) = Right $ Stmt src (Sequence src []) : rest
braceParser _ = Left $ NotMatched defaultSource

literalParser :: Parser
literalParser (Terminal (IntLiteral src i) : rest) = Right $ Expr src (IntValue src i) : rest
literalParser (Terminal (FloatLiteral src i) : rest) = Right $ Expr src (FloatValue src i) : rest
literalParser (Terminal (BoolLiteral src i) : rest) = Right $ Expr src (BoolValue src i) : rest
literalParser (Terminal (StringLiteral src i) : rest) = Right $ Expr src (StringValue src (processEscapes i)) : rest
literalParser (Terminal (NilLiteral src) : rest) = Right $ Expr src (NilValue src) : rest
literalParser (Terminal (CharLiteral src i) : rest) = do
    case processEscapes i of
        [c] -> Right $ Expr src (CharValue src c) : rest
        [] -> Left $ EmptyChar src
        (_ : _) -> Left $ CharTooLong src
literalParser _ = Left $ NotMatched defaultSource

sequenceParser :: Parser
sequenceParser (s1@(Stmt src _) : s2@(Stmt _ _) : rest) = Right $ Sequence src [s1, s2] : rest
sequenceParser (Sequence src se : s@(Stmt _ _) : rest) = Right $ Sequence src (se ++ [s]) : rest
sequenceParser _ = Left $ NotMatched defaultSource

stmtParser :: Parser
stmtParser (s@(Sequence src _) : rest) = Right $ Stmt src s : rest
stmtParser _ = Left $ NotMatched defaultSource

opParser :: Parser
opParser (Terminal (NotSign src) : rest) =
    Right $ UnOperation src Not : rest
opParser (Terminal (MinusSign src) : rest) =
    Right $ UnOperation src Neg : rest
opParser (Terminal (PlusSign src) : rest) =
    Right $ BinOperation src Plus : rest
opParser (e@(Expr _ _) : Terminal (MinusSign src) : rest) =
    Right $ e : BinOperation src Minus : rest
opParser (Terminal (Star src) : rest) =
    Right $ BinOperation src Times : rest
opParser (Terminal (RightBar src) : rest) =
    Right $ BinOperation src Divide : rest
opParser (Terminal (AndSign src) : rest) =
    Right $ BinOperation src AndOp : rest
opParser (Terminal (OrSign src) : rest) =
    Right $ BinOperation src OrOp : rest
opParser (Terminal (OpenChevron src) : e@(Expr _ _) : rest) =
    Right $ BinOperation src Lt : e : rest
opParser (Terminal (CloseChevron src) : e@(Expr _ _) : rest) =
    Right $ BinOperation src Gt : e : rest
opParser (Terminal (GeSign src) : rest) =
    Right $ BinOperation src Ge : rest
opParser (Terminal (LeSign src) : rest) =
    Right $ BinOperation src Le : rest
opParser (Terminal (EqSign src) : rest) =
    Right $ BinOperation src Eq : rest
opParser (Terminal (NeqSign src) : rest) =
    Right $ BinOperation src Neq : rest
opParser (Terminal (Collon src) : rest) =
    Right $ BinOperation src Concat : rest
opParser (Expr _ i : BinOperation src o : Expr _ j : rest) =
    Right $ Expr src (BinOp src o i j) : rest
opParser (UnOperation src o : Expr _ i : rest) =
    Right $ Expr src (UnOp src o i) : rest
opParser _ = Left $ NotMatched defaultSource

-- loops
whileParser :: Parser
whileParser
    ( Terminal (While src)
            : Expr _ b
            : Terminal (Do _)
            : Stmt _ e
            : rest
        ) = Right $ Stmt src (WhileLoop src b e) : rest
whileParser _ = Left $ NotMatched defaultSource

ifParser :: Parser
ifParser
    ( Terminal (If src)
            : Expr _ eif
            : Terminal (Then _)
            : Stmt _ ethen
            : Terminal (Else _)
            : Stmt _ eelse
            : rest
        ) = Right $ Stmt src (Conditional src eif ethen eelse) : rest
ifParser _ = Left $ NotMatched defaultSource

functionParser :: Parser
functionParser
    ( TypeDeclaration src t
            : FunctionName _ x
            : Args sargs s
            : Stmt _ e
            : rest
        ) = Right $ Stmt src (Function src x t (Args sargs s) e) : rest
functionParser
    ( Terminal (Return src)
            : Expr _ e
            : Terminal (EndStatement _)
            : rest
        ) = Right $ Stmt src (ReturnVal src e) : rest
functionParser _ = Left $ NotMatched defaultSource

argsParser :: Parser
-- declaring
argsParser
    ( Args src as
            : Terminal (OpenParens _)
            : Terminal (CloseParens _)
            : rest
        ) = Right $ Args src as : rest
argsParser
    ( Args src as
            : Terminal (OpenParens _)
            : TypeDeclaration tsrc t
            : Var _ x _
            : Terminal (CloseParens _)
            : rest
        ) = Right $ Args src (as ++ [Arg tsrc t x]) : rest
argsParser
    ( Args src as
            : Terminal (OpenParens _)
            : TypeDeclaration tsrc t
            : Var _ x _
            : Terminal (Comma _)
            : rest
        ) = Right $ Args src (as ++ [Arg tsrc t x]) : Terminal (OpenParens src) : rest
argsParser
    ( typedec@(TypeDeclaration _ _)
            : fName@(FunctionName src _)
            : parens@(Terminal (OpenParens _))
            : rest
        ) = Right $ typedec : fName : Args src [] : parens : rest
-- calling
argsParser
    ( CallArgs src as
            : Terminal (OpenParens _)
            : Terminal (CloseParens _)
            : rest
        ) = Right $ CallArgs src as : rest
argsParser
    ( CallArgs src as
            : Terminal (OpenParens _)
            : Expr esrc e
            : Terminal (CloseParens _)
            : rest
        ) = Right $ CallArgs src (as ++ [CallArg esrc e]) : rest
argsParser
    ( CallArgs src as
            : Terminal (OpenParens _)
            : Expr esrc e
            : Terminal (Comma _)
            : rest
        ) = Right $ CallArgs src (as ++ [CallArg esrc e]) : Terminal (OpenParens src) : rest
argsParser
    ( fName@(FunctionName src _)
            : parens@(Terminal (OpenParens _))
            : rest
        ) = Right $ fName : CallArgs src [] : parens : rest
argsParser _ = Left $ NotMatched defaultSource

callParser :: Parser
callParser
    ( FunctionName _ _
            : CallArgs _ _
            : Terminal (OpenParens _)
            : _
        ) = Left $ NotMatched defaultSource
callParser
    ( FunctionName sf f
            : CallArgs sas as
            : rest
        ) = Right $ Expr sf (Call sf f (CallArgs sas as)) : rest
callParser (Expr _ f@(Call src _ _) : Terminal (EndStatement _) : rest) = Right $ Stmt src f : rest
callParser _ = Left $ NotMatched defaultSource

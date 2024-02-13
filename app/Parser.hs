module Parser where

import Tokens

type Parser = [Token] -> [Token]

-- final parser
parsers :: [Parser]
parsers =
    [ ifParser
    , whileParser
    , callParser
    , functionParser
    , sequenceParser
    , braceParser
    , opParser
    , varParser
    , literalParser
    ]

-- main parser that composes the other individual parsers
mainParse :: Parser
mainParse tokens = unwind $ filter (notElem ExprErr) [par tokens | par <- parsers]
  where
    unwind (x : _) = x
    unwind [] = [ExprErr]

-- parse function
parse :: [Token] -> IO Token
parse [e] = return e
-- parsePipe :: [Token] -> Token
-- parsePipe [e] = e
parse express = do
    -- print $ "parsed" ++ show parsed
    -- print $ "filtered:" ++ show filtered
    -- print $ "new:" ++ show newExpr
    parse newExpr
  where
    parseFrom i = take i express ++ mainParse (drop i express)
    unwind (x : _) = x
    unwind [] = [ExprErr]
    parsed = zipWith (\i _ -> parseFrom i) [0 ..] express
    filtered = filter (notElem ExprErr) parsed
    newExpr = unwind filtered


-- Defining each individual parser
braceParser :: Parser
braceParser
    ( Terminal OpenParens
            : Expr i
            : Terminal CloseParens
            : rest
        ) = Expr i : rest
braceParser
    ( Terminal OpenBrace
            : Expr i
            : Terminal CloseBrace
            : rest
        ) = Expr (Sequence [i]) : rest
braceParser
    ( Terminal OpenBrace
            : Sequence s
            : Terminal CloseBrace
            : rest
        ) = Expr (Sequence s) : rest
braceParser
    ( Terminal OpenBrace
            : Terminal CloseBrace
            : rest
        ) = Expr (Sequence []) : rest
braceParser _ = [ExprErr]

literalParser :: Parser
literalParser (Terminal (IntLiteral i) : rest) = Expr (IntValue i) : rest
literalParser (Terminal (FloatLiteral i) : rest) = Expr (FloatValue i) : rest
literalParser (Terminal (BoolLiteral i) : rest) = Expr (BoolValue i) : rest
literalParser _ = [ExprErr]

sequenceParser :: Parser
sequenceParser (Expr i : Expr j : rest) = Sequence [i, j] : rest
sequenceParser (Sequence s : Expr i : rest) = Sequence (s ++ [i]) : rest
sequenceParser (Expr j : Sequence s : rest) = Sequence (j : s) : rest
sequenceParser _ = [ExprErr]

isUnary :: Op -> Bool
isUnary x = x `elem` [Not, Minus]

isBinary :: Op -> Bool
isBinary x = x /= Not

opParser :: Parser
opParser (Expr i : Terminal (Operator o) : Expr j : rest) =
    if isBinary o then Expr (BinOp o i j) : rest else [ExprErr]
opParser (Var i : Terminal (Operator o) : Expr j : rest) =
    if isBinary o then Expr (BinOp o (Var i) j) : rest else [ExprErr]
opParser (Var i : Terminal (Operator o) : Var j : rest) =
    if isBinary o then Expr (BinOp o (Var i) (Var j)) : rest else [ExprErr]
opParser (Expr i : Terminal (Operator o) : Var j : rest) =
    if isBinary o then Expr (BinOp o i (Var j)) : rest else [ExprErr]
opParser (Terminal (Operator o) : Expr i : rest) =
    if isUnary o then Expr (UnOp o i) : rest else [ExprErr]
opParser (Terminal (Operator o) : Var i : rest) =
    if isUnary o then Expr (UnOp o (Var i)) : rest else [ExprErr]
opParser _ = [ExprErr]

varParser :: Parser
varParser
    ( Var x
            : Terminal Assigner
            : Expr e
            : Terminal EndStatement
            : rest
        ) = Expr (Assign x e) : rest
varParser
    ( Var x
            : Terminal Assigner
            : Var y
            : Terminal EndStatement
            : rest
        ) = Expr (Assign x (Var y)) : rest
varParser
    ( Terminal (Type t)
            : Var x
            : Terminal EndStatement
            : rest
        ) = Expr (Declare t x) : rest
varParser
    ( Terminal (Identifier x)
            : rest
        ) = Var x : rest
varParser _ = [ExprErr]

whileParser :: Parser
whileParser
    ( Terminal While
            : Expr b
            : Terminal Do
            : Expr e
            : rest
        ) = Expr (WhileLoop b e) : rest
whileParser _ = [ExprErr]

ifParser :: Parser
ifParser
    ( Terminal If
            : Expr eif
            : Terminal Then
            : Expr ethen
            : Terminal Else
            : Expr eelse
            : rest
        ) = Expr (Conditional eif ethen eelse) : rest
ifParser _ = [ExprErr]

functionParser :: Parser
-- arguments
functionParser
    ( Terminal OpenBracket
            : Terminal (Type t)
            : Var x
            : Terminal CloseBracket
            : rest
        ) = Arg t x : rest
functionParser (Arg ta a : rest) = Args [Arg ta a] : rest
functionParser (Args s : Arg ta a : rest) = Args (s ++ [Arg ta a]) : rest
-- body
functionParser
    ( Terminal (Type t)
            : Var x
            : Args s
            : Expr e
            : rest
        ) = Expr (Function x t (Args s) e) : rest
functionParser
    ( Terminal (Type t)
            : Var x
            : Expr e
            : rest
        ) = Expr (Function x t (Args []) e) : rest
functionParser
    ( Terminal Return
            : Expr e
            : Terminal EndStatement
            : rest
        ) = Expr (ReturnVal e) : rest
functionParser
    ( Terminal Return
            : Var x
            : Terminal EndStatement
            : rest
        ) = Expr (ReturnVal (Var x)) : rest
functionParser _ = [ExprErr]

callParser :: Parser
callParser
    ( Terminal OpenBracket
            : Terminal CloseBracket
            : rest
        ) = CallArgs [] : rest
callParser
    ( Terminal OpenBracket
            : Expr e
            : Terminal CloseBracket
            : rest
        ) = CallArg e : rest
callParser
    ( Terminal OpenBracket
            : Var x
            : Terminal CloseBracket
            : rest
        ) = CallArg (Var x) : rest
callParser (CallArg a : rest) = CallArgs [a] : rest
callParser (CallArgs as : CallArg a : rest) = CallArgs (as ++ [a]) : rest
callParser
    ( Var x
            : Terminal Assigner
            : Var f
            : CallArgs as
            : Terminal EndStatement
            : rest
        ) = Expr (Assign x (Call f (CallArgs as))) : rest
callParser
    ( Var f
            : CallArgs as
            : Terminal EndStatement
            : rest
        ) = Expr (Call f (CallArgs as)) : rest
callParser _ = [ExprErr]

module Parser where

import Lexer

{- PARSER -}
data Token
    = Expr Token
    | Sequence [Token]
    | Var String
    | Arg Types String
    | Args [Token]
    | Function String Types Token Token
    | CallArg Token
    | CallArgs [Token]
    | Call String Token
    | ReturnVal Token
    | Conditional Token Token Token
    | WhileLoop Token Token
    | Declare Types String
    | Assign String Token
    | BinOp Op Token Token
    | UnOp Op Token
    | Terminal TerminalToken
    | IntValue Int
    | FloatValue Float
    | BoolValue Bool
    | ExprErr
    deriving (Show, Eq)

parsePipe :: [Token] -> IO Token
parsePipe [e] = return e
-- parsePipe :: [Token] -> Token
-- parsePipe [e] = e
parsePipe express = do
    -- print $ "filtered:" ++ show filtered
    -- print $ "new:" ++ show newExpr
    parsePipe newExpr
  where
    parsing i =
        let (expr, after) = parse (drop i express)
         in (take i express, expr, after)
    parsed = zipWith (\i _ -> parsing i) [0 ..] express
    filtered = filter (\(_, ee, _) -> ee /= ExprErr) parsed
    newExpr =
        if null filtered
            then [ExprErr]
            else let (b, e, a) = head filtered in b ++ [e] ++ a

unaryOperators :: [Op]
unaryOperators = [Not, Minus]
notBinaryOperators :: [Op]
notBinaryOperators = [Not]

-- production rules but inverted
parse :: [Token] -> (Token, [Token])
parse
    ( Terminal OpenBracket
            : Terminal (Type t)
            : Var x
            : Terminal CloseBracket
            : rest
        ) = (Arg t x, rest)
parse (Arg ta a : rest) = (Args [Arg ta a], rest)
parse (Args s : Arg ta a : rest) = (Args (s ++ [Arg ta a]), rest)
parse
    ( Terminal (Type t)
            : Var x
            : Args s
            : Expr e
            : rest
        ) =
        (Expr (Function x t (Args s) e), rest)
parse
    ( Terminal (Type t)
            : Var x
            : Expr e
            : rest
        ) =
        (Expr (Function x t (Args []) e), rest)
parse
    ( Terminal OpenBracket
            : Terminal CloseBracket
            : rest
        ) = (CallArgs [], rest)
parse
    ( Terminal OpenBracket
            : Expr e
            : Terminal CloseBracket
            : rest
        ) = (CallArg e, rest)
parse
    ( Terminal OpenBracket
            : Var v
            : Terminal CloseBracket
            : rest
        ) = (CallArg (Var v), rest)
parse
    (CallArg a : rest) = (CallArgs [a], rest)
parse (CallArgs as : CallArg a : rest) = (CallArgs (as ++ [a]), rest)
parse
    ( Var x
            : Terminal Assigner
            : Var f
            : CallArgs as
            : Terminal EndStatement
            : rest
        ) = (Expr (Assign x (Call f (CallArgs as))), rest)
parse
    ( Var f
            : CallArgs as
            : Terminal EndStatement
            : rest
        ) = (Expr (Call f (CallArgs as)), rest)
parse (Terminal Return : Expr e : Terminal EndStatement : rest) = (Expr (ReturnVal e), rest)
parse (Terminal Return : Var x : Terminal EndStatement : rest) = (Expr (ReturnVal (Var x)), rest)
parse
    ( Terminal If
            : Expr eif
            : Terminal Then
            : Expr ethen
            : Terminal Else
            : Expr eelse
            : rest
        ) =
        (Expr (Conditional eif ethen eelse), rest)
parse
    ( Terminal While
            : Expr b
            : Terminal Do
            : Expr e
            : rest
        ) = (Expr (WhileLoop b e), rest)
parse (Terminal OpenBrace : Terminal CloseBrace : rest) = (Expr (Sequence []), rest)
parse (Terminal (Identifier x) : rest) = (Var x, rest)
parse (Terminal (Type t) : Var x : Terminal EndStatement : rest) = (Expr (Declare t x), rest)
parse (Var x : Terminal Assigner : Expr e : Terminal EndStatement : rest) = (Expr (Assign x e), rest)
parse (Var x : Terminal Assigner : Var y : Terminal EndStatement : rest) = (Expr (Assign x (Var y)), rest)
parse (Expr i : Terminal (Operator o) : Expr j : rest) =
    if o `elem` notBinaryOperators then (ExprErr, []) else (Expr (BinOp o i j), rest)
parse (Var i : Terminal (Operator o) : Expr j : rest) =
    if o `elem` notBinaryOperators then (ExprErr, []) else (Expr (BinOp o (Var i) j), rest)
parse (Expr i : Terminal (Operator o) : Var j : rest) =
    if o `elem` notBinaryOperators then (ExprErr, []) else (Expr (BinOp o i (Var j)), rest)
parse (Var i : Terminal (Operator o) : Var j : rest) =
    if o `elem` notBinaryOperators then (ExprErr, []) else (Expr (BinOp o (Var i) (Var j)), rest)
parse (Terminal (Operator o) : Var j : rest) =
    if o `elem` unaryOperators then (Expr (UnOp o (Var j)), rest) else (ExprErr, [])
parse (Terminal (Operator o) : Expr j : rest) =
    if o `elem` unaryOperators then (Expr (UnOp o j), rest) else (ExprErr, [])
parse (Terminal OpenParens : Expr i : Terminal CloseParens : rest) = (Expr i, rest)
parse (Terminal OpenBrace : Expr i : Terminal CloseBrace : rest) = (Expr (Sequence [i]), rest)
parse (Terminal OpenBrace : Sequence s : Terminal CloseBrace : rest) = (Expr (Sequence s), rest)
parse (Terminal (IntLiteral i) : rest) = (Expr (IntValue i), rest)
parse (Terminal (FloatLiteral i) : rest) = (Expr (FloatValue i), rest)
parse (Terminal (BoolLiteral i) : rest) = (Expr (BoolValue i), rest)
parse (Expr i : Expr j : rest) = (Sequence [i, j], rest)
parse (Sequence s : Expr i : rest) = (Sequence (s ++ [i]), rest)
parse (Expr j : Sequence s : rest) = (Sequence (j : s), rest)
parse _ = (ExprErr, [])

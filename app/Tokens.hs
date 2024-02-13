module Tokens where

import Data.List (intercalate)

data Types = TInt | TFloat | TBool | TNil
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
    = OpenBrace
    | CloseBrace
    | OpenParens
    | CloseParens
    | OpenBracket
    | CloseBracket
    | EndStatement
    | Return
    | Assigner
    | If
    | Then
    | Else
    | While
    | Do
    | Type Types
    | Operator Op
    | IntLiteral Int
    | FloatLiteral Float
    | BoolLiteral Bool
    | Identifier [Char]
    deriving (Show, Eq)

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
    deriving (Eq)

instance Show Token where
    show t = intercalate "\n" (showAst t)

-- utility
addNewLine :: [String] -> [String]
addNewLine = map ("  " ++)

push :: [Token] -> [String]
push = concatMap (addNewLine . showAst)

showAst :: Token -> [String]
showAst (Expr t) = "Expr:" : addNewLine (showAst t)
showAst (Sequence s) = "Sequence: " : push s
showAst (Var x) = ["Var " ++ x]
showAst (Arg t s) = ["Arg " ++ show t ++ " " ++ s]
showAst (Args as) = "Args " : push as
showAst (Function name typ args body) =
    ("Function " ++ show typ ++ " " ++ name) : push [args, body]
showAst (CallArg a) = "CallArg " : push [a]
showAst (CallArgs as) = "CallArgs " : push as
showAst (Call name args) = ("Call " ++ name) : push [args]
showAst (ReturnVal v) = "Return " : push [v]
showAst (Conditional cond tr fl) =
    ["Conditional "]
        ++ addNewLine ("If: " : push [cond])
        ++ addNewLine ("Then: " : push [tr])
        ++ addNewLine ("Else: " : push [fl])
showAst (WhileLoop cond body) =
    ["While "]
        ++ addNewLine ("Cond: " : push [cond])
        ++ addNewLine ("Do: " : push [body])
showAst (Declare t name) = ["Declare " ++ show t ++ " " ++ name]
showAst (Assign name val) = ("Assign " ++ name) : push [val]
showAst (BinOp op i j) = ("BinOP " ++ show op) : push [i, j]
showAst (UnOp op i) = ("UnOp " ++ show op) : push [i]
showAst (Terminal t) = ["TERMINAL " ++ show t]
showAst (IntValue i) = ["Int " ++ show i]
showAst (FloatValue f) = ["Float " ++ show f]
showAst (BoolValue b) = ["Bool " ++ show b]
showAst ExprErr = ["ERROR"]

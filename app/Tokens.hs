module Tokens where

import Data.List (intercalate)
data Source = Source
    { line :: Int
    , col :: Int
    }
    deriving (Show, Eq)

defaultSource :: Source
defaultSource =
    Source
        { line = 0
        , col = 0
        }

data Types 
    = TInt 
    | TFloat 
    | TBool 
    | TNil
    | TString
    | TList Types
    deriving (Show, Eq)

data BinOperator
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
    | Concat
    deriving (Show, Eq)

data UnOperator
    = Not 
    | Neg
    deriving (Show, Eq)

data TerminalToken
    = OpenBrace Source
    | CloseBrace Source
    | OpenParens Source
    | CloseParens Source
    | OpenBracket Source
    | CloseBracket Source
    | OpenChevron Source
    | CloseChevron Source
    | EndStatement Source
    | Comma Source
    | Return Source
    | Assigner Source
    | If Source
    | Then Source
    | Else Source
    | While Source
    | Do Source
    | EqSign Source
    | NeqSign Source
    | GeSign Source
    | LeSign Source
    | NotSign Source
    | PlusSign Source
    | MinusSign Source
    | Star Source
    | RightBar Source
    | Collon Source
    | ListType Source
    | IntType Source
    | FloatType Source
    | BoolType Source
    | NilType Source
    | StringType Source
    | IntLiteral Source Int
    | FloatLiteral Source Float
    | BoolLiteral Source Bool
    | StringLiteral Source String
    | Identifier Source [Char]
    deriving (Show, Eq)

data Token
    = Expr Source Token
    | Stmt Source Token
    | Sequence Source [Token]
    | Var Source String
    | FunctionName Source String
    | Arg Source Types String
    | Args Source [Token]
    | Function Source String Types Token Token
    | CallArg Source Token
    | CallArgs Source [Token]
    | Call Source String Token
    | ReturnVal Source Token
    | Conditional Source Token Token Token
    | WhileLoop Source Token Token
    | Declare Source Types String
    | Assign Source String Token
    | BinOperation Source BinOperator
    | UnOperation Source UnOperator
    | BinOp Source BinOperator Token Token
    | UnOp Source UnOperator Token
    | TypeDeclaration Source Types
    | IntValue Source Int
    | FloatValue Source Float
    | BoolValue Source Bool
    | StringValue Source String
    | NilValue Source
    | ListValue Source Types [Token]
    | Terminal TerminalToken
    deriving (Eq)

data Variable
    = VInt Int
    | VBool Bool
    | VFloat Float
    | VString String
    | VNil
    | VList Types [Variable]
    deriving (Show, Eq)

getDefaultValue :: Types -> Variable
getDefaultValue TInt = VInt 0
getDefaultValue TFloat =  VFloat 0
getDefaultValue TBool =  VBool True
getDefaultValue TNil = VNil
getDefaultValue TString = VString ""
getDefaultValue (TList t) =  VList t []

getType :: Variable -> Types
getType (VInt _) = TInt
getType (VFloat _) = TFloat
getType (VBool _) = TBool
getType (VString _) = TString
getType VNil = TNil
getType (VList t _) = TList t

instance Show Token where
    show t = intercalate "\n" (showAst t)

-- utility
addNewLine :: [String] -> [String]
addNewLine = map ("  " ++)

push :: [Token] -> [String]
push = concatMap (addNewLine . showAst)

showAst :: Token -> [String]
showAst (Expr _ t) = "Expr:" : addNewLine (showAst t)
showAst (Stmt _ t) = "Stmt:" : addNewLine (showAst t)
showAst (Sequence _ s) = "Sequence: " : push s
showAst (Var _ x) = ["Var " ++ x]
showAst (FunctionName _ x) = ["FuncName " ++ x]
showAst (Arg _ t s) = ["Arg " ++ show t ++ " " ++ s]
showAst (Args _ as) = "Args " : push as
showAst (Function _ name typ args body) =
    ("Function " ++ show typ ++ " " ++ name) : push [args, body]
showAst (CallArg _ a) = "CallArg " : push [a]
showAst (CallArgs _ as) = "CallArgs " : push as
showAst (Call _ name args) = ("Call " ++ name) : push [args]
showAst (ReturnVal _ v) = "Return " : push [v]
showAst (Conditional _ cond tr fl) =
    ["Conditional "]
        ++ addNewLine ("If: " : push [cond])
        ++ addNewLine ("Then: " : push [tr])
        ++ addNewLine ("Else: " : push [fl])
showAst (WhileLoop _ cond body) =
    ["While "]
        ++ addNewLine ("Cond: " : push [cond])
        ++ addNewLine ("Do: " : push [body])
showAst (Declare _ t name) = ["Declare (" ++ show t ++ ") " ++ name]
showAst (Assign _ name val) = ("Assign " ++ name) : push [val]
showAst (BinOperation _ op) = ["BinOperation " ++ show op]
showAst (UnOperation _ op) = ["UnOperation " ++ show op]
showAst (BinOp _ op i j) = ("BinOP " ++ show op) : push [i, j]
showAst (UnOp _ op i) = ("UnOp " ++ show op) : push [i]
showAst (Terminal t) = ["TERMINAL " ++ show t]
showAst (IntValue _ i) = ["Int " ++ show i]
showAst (FloatValue _ f) = ["Float " ++ show f]
showAst (BoolValue _ b) = ["Bool " ++ show b]
showAst (StringValue _ s) = ["String " ++ s]
showAst (NilValue _) = ["NILVAL"]
showAst (ListValue _ t vals) = ("List <|" ++ show t ++ "|>" ): push vals
showAst (TypeDeclaration _ t ) = ["TypeDecl " ++ show t ]

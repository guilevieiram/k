module Semantic where

import Control.Monad.State
import Data.Either (isLeft, isRight)
import Data.List
import States
import SystemFunctions
import Tokens

data SemanticError
    = SemanticError
    | TerminalTokenFound TerminalToken
    | InvalidOperation Source String
    | UndefinedVariable Source String
    | InvalidAssignment Source String
    | InvalidSequence Source String
    | ReturnTypesUnmatch Source String
    | ConditionalError Source String
    | BranchNotNil Source String
    | ArgumentsError Source String
    | ReturnOutsideFunction Source String
    | ReturnWithWrongType Source String
    | UndefinedFunction Source String
    | NumberOfArgumentsUnmatch Source String
    | CallingArgumentWrongType Source String
    | GenericError Source String
    | ConcatenationWithWrongType Source String
    | NotImplemented
    deriving (Show, Eq)

-- entrypoint
analyse :: Token -> Either SemanticError Types
analyse token = result
  where
    (result, _) = runState (typeOf token) initialStack
    sysFuncs = map fToken sysFunctions
    initialStack = StateStack{vars = [], defFuncs = [], funcs = sysFuncs}

{- main semantic analyser -}
typeOf :: Token -> State StateStack (Either SemanticError Types)
-- Constants
typeOf (IntValue _ _) = return $ Right TInt
typeOf (FloatValue _ _) = return $ Right TFloat
typeOf (BoolValue _ _) = return $ Right TBool
typeOf (StringValue _ _) = return $ Right TString
-- unary operations
typeOf (UnOp src Not t) = do
    tType <- typeOf t
    case tType of
        Left err -> return $ Left err
        Right TBool -> return $ Right TBool
        Right _ ->
            return . Left $ InvalidOperation src "Notting a non-boolean expression."
typeOf (UnOp src Neg t) = do
    tType <- typeOf t
    case tType of
        Left err -> return $ Left err
        Right ty ->
            return $
                if isNumeric ty
                    then Right ty
                    else Left $ InvalidOperation src "Negating a non-numeric expression."

-- binary Operations
typeOf (BinOp src o x y) = do
    xType <- typeOf x
    yType <- typeOf y
    return $ ret o xType yType
  where
    inner (TList t) = Right t
    inner _ = Left $ ConcatenationWithWrongType src "Concat applied to a non-list type"
    ret _ _ (Left e) = Left e
    ret _ (Left e) _ = Left e
    ret op (Right xt) (Right yt)
        | op `elem` [Plus, Minus, Times, Divide] && xt == yt && isNumeric xt = Right xt
        | op `elem` [Gt, Ge, Lt, Le, Eq, Neq] && xt == yt && isNumeric xt = Right TBool
        | op == Concat && isList xt = do
            inT <- inner xt
            if inT == yt
                then Right xt
                else
                    Left $
                        ConcatenationWithWrongType src "Concat with wrong inner type"
        | otherwise = Left $ InvalidOperation src "Operation not valid"

-- declarations
typeOf (Declare _ ty x) = do
    stacks <- get
    let varStack = vars stacks
    put $ stacks{vars = (x, ty) : varStack}
    return . Right $ TNil

-- assignments
typeOf (Assign src x tok) = do
    stacks <- get
    let varStack = vars stacks
    tokType <- typeOf tok
    let varType = getVar varStack x
    return $ case tokType of
        Left err -> Left err
        Right t1 -> decideType t1 varType
  where
    decideType _ Nothing = Left $ UndefinedVariable src x
    decideType t1 (Just t2) =
        if t1 == t2
            then Right TNil
            else Left $ InvalidAssignment src "Assignment of wrong type"

-- Expressions
typeOf (Expr _ tok) = do typeOf tok
-- Statements
typeOf (Stmt _ tok) = do
    innerE <- typeOf tok
    case innerE of
        Left err -> return . Left $ err
        Right _ -> return . Right $ TNil
-- Sequence
typeOf (Sequence _ toks) = do
    states <- mapM typeOf toks
    return $
        if not (all isRight states)
            then head (filter isLeft states)
            else Right TNil

-- Variables
typeOf (Var src x) = do
    stacks <- get
    let varStack = vars stacks
    let xType = getVar varStack x
    return $ case xType of
        Nothing -> Left $ UndefinedVariable src "Use of undefined variable"
        Just t -> Right t

-- Conditionals
typeOf (Conditional src cond tr fl) = do
    tCond <- typeOf cond
    tTrue <- typeOf tr
    tFalse <- typeOf fl
    return $ case (tCond, tTrue, tFalse) of
        (Right TBool, Right TNil, Right TNil) -> Right TNil
        (Left err, _, _) -> Left err
        (_, Left err, _) -> Left err
        (_, _, Left err) -> Left err
        (Right _, _, _) -> Left $ ConditionalError src "Need to resolve to boolean."

-- while loop
typeOf (WhileLoop src cond branch) = do
    tCond <- typeOf cond
    tBranch <- typeOf branch
    return $ case (tCond, tBranch) of
        (Right TBool, _) -> Right TNil
        (Right _, _) -> Left $ ConditionalError src "Condition does not resolve to boolean"
        (Left err, _) -> Left err

-- Function definition
typeOf func@(Function src _ _ args@(Args _ as) body) = do
    stacks <- get
    let defFuncStack = defFuncs stacks
    let varStack = vars stacks
    let funcStack = funcs stacks
    put $
        stacks
            { vars = [(x, t) | (Arg _ t x) <- as] ++ varStack
            , defFuncs = func : defFuncStack
            , funcs = func : funcStack
            }

    bodyType <- typeOf body
    argsType <- typeOf args
    let res = proc bodyType argsType
    put $ stacks{vars = varStack, defFuncs = defFuncStack, funcs = func : funcStack}
    return res
  where
    proc (Right TNil) (Right TNil) = Right TNil
    proc (Right _) _ = Left $ BranchNotNil src "Function body not a nil expression."
    proc _ (Left err) = Left err
    proc (Left err) _ = Left err

-- func arguments
typeOf (Args src as) = do
    let varNames = [name | (Arg _ _ name) <- as]
    return $
        if hasDuplicates varNames
            then Left $ ArgumentsError src "Duplicate argument names"
            else Right TNil
  where
    hasDuplicates list = length list /= length (nub list)
typeOf (Arg _ t _) = return $ Right t
-- return
typeOf (ReturnVal src val) = do
    stacks <- get
    let defFuncStack = defFuncs stacks
    rType <- typeOf val
    return $ case defFuncStack of
        [] -> Left $ ReturnOutsideFunction src "Return statement is outside function definition."
        (func : _) -> proc func rType
  where
    proc (Function _ _ funcType _ _) (Right retType) =
        if funcType == retType
            then Right TNil
            else Left $ ReturnWithWrongType src "Bad type of returned expression"
    proc _ (Left err) = Left err
    proc _ _ = Left $ GenericError src "Non-function token found on function stack."

-- calling function
typeOf (Call src funcName (CallArgs _ args)) = do
    stacks <- get
    let funcStack = funcs stacks
    let func = getFunc funcStack funcName
    argsTypes <- mapM typeOf args
    case func of
        Nothing -> return . Left $ UndefinedFunction src "Function not defined in scope."
        Just (Function _ _ fType (Args _ fArgs) _) -> do
            fArgsTypes <- mapM typeOf fArgs
            case matchArgs src fArgsTypes argsTypes of
                Left err -> return . Left $ err
                Right _ -> return . Right $ fType
        (Just _) ->
            return . Left $
                GenericError src "Something other then function on the functions stack."
typeOf (CallArg _ val) = do typeOf val

-- clear errors
typeOf (Terminal t) = return . Left $ TerminalTokenFound t
typeOf _ = return $ Left SemanticError

-- utility functions
matchArgs ::
    Source ->
    [Either SemanticError Types] ->
    [Either SemanticError Types] ->
    Either SemanticError Types
matchArgs src fArgs cArgs
    | length fArgs /= length cArgs =
        Left $
            NumberOfArgumentsUnmatch src "Number of args in call does not match function definition"
    | not (all isRight fArgs) = head (filter isLeft fArgs)
    | not (all isRight cArgs) = head (filter isLeft cArgs)
    | and (zipWith (==) fArgs cArgs) = Right TNil
    | otherwise = case findFirstDifferent fArgs cArgs of
        Nothing -> Right TNil
        Just (t1, t2) ->
            Left $
                CallingArgumentWrongType
                    src
                    ( "Calling value and function argument types differ: "
                        ++ "function: "
                        ++ show t1
                        ++ " - Calling "
                        ++ show t2
                    )

findFirstDifferent :: (Eq a) => [a] -> [a] -> Maybe (a, a)
findFirstDifferent [] _ = Nothing
findFirstDifferent _ [] = Nothing
findFirstDifferent (x : xs) (y : ys)
    | x /= y = Just (x, y)
    | otherwise = findFirstDifferent xs ys

-- util functions
isNumeric :: Types -> Bool
isNumeric t = t `elem` [TFloat, TInt]

isList :: Types -> Bool
isList (TList _) = True
isList _ = False

getVar :: VariablesStack -> String -> Maybe Types
getVar stack name = find ((name ==) . fst) stack >>= Just . snd

putVar :: VariablesStack -> (String, Types) -> VariablesStack
putVar stack (name, t) = (name, t) : stack

getFunc :: FunctionStack -> String -> Maybe Token
getFunc (f@(Function _ funcName _ _ _) : fs) name =
    if name == funcName
        then Just f
        else getFunc fs name
getFunc _ _ = Nothing

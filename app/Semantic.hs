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
    | IndexingError Source String
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
typeOf (CharValue _ _) = return $ Right TChar
typeOf (NilValue _) = return $ Right TNil
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
    ret _ _ (Left e) = Left e
    ret _ (Left e) _ = Left e
    ret op (Right xt) (Right yt)
        | op `elem` [Plus, Minus, Times, Divide] && xt == yt && isNumeric xt = Right xt
        | op `elem` [AndOp, OrOp] && xt == yt && xt == TBool = Right TBool
        | op `elem` [Eq, Neq] && xt == yt = Right TBool
        | op `elem` [Gt, Ge, Lt, Le] && xt == yt && isNumeric xt = Right TBool
        | op == Concat && xt == yt = case xt of
            TString -> Right TString
            TArray t -> Right (TArray t)
            _ -> Left $ InvalidOperation src "Operation not valid"
        | otherwise = Left $ InvalidOperation src "Operation not valid"

-- declarations
typeOf (Declare _ ty x) = do
    stacks <- get
    let varStack = vars stacks
    put $ stacks{vars = (x, ty) : varStack}
    return . Right $ TNil

-- assignments
typeOf (Assign src var@(Var{}) tok) = do
    tokType <- typeOf tok
    varType <- typeOf var
    return $ case (varType, tokType) of
        (Right vt, Right et) -> decideType et vt
        (Left err, _) -> Left err
        (_, Left err) -> Left err
  where
    decideType et xt =
        if isSubtype xt et
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
typeOf (Var src x as) = do
    stacks <- get
    let varStack = vars stacks
    let xType = getVar varStack x
    case xType of
        Nothing -> return . Left $ UndefinedVariable src "Use of undefined variable"
        Just t -> compute as t
    where 
        compute (e: es) (TArray t) = do
            aType <- typeOf e
            case aType of
                Left err -> return $ Left err
                Right TInt -> compute es t 
                Right other -> return $ Left $ IndexingError src ("index did not evaluate to int. Got : " ++ show other)
        compute [] t = return $ Right t
        compute _ t = return .Left $ IndexingError src ("Trying to index a non-array: " ++ show t)

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
        if isSubtype funcType retType
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
    | otherwise = case findFirstDifferent isSub fArgs cArgs of
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
  where
    isSub x y = case liftM2 isSubtype x y of
        Left _ -> False
        Right val -> val

findFirstDifferent :: (Eq a) => (a -> a -> Bool) -> [a] -> [a] -> Maybe (a, a)
findFirstDifferent _ [] _ = Nothing
findFirstDifferent _ _ [] = Nothing
findFirstDifferent comp (x : xs) (y : ys)
    | not $ comp x y = Just (x, y)
    | otherwise = findFirstDifferent comp xs ys

-- util functions
isNumeric :: Types -> Bool
isNumeric t = t `elem` [TFloat, TInt]

isArray :: Types -> Bool
isArray (TArray _) = True
isArray _ = False

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

isSubtype :: Types -> Types -> Bool
isSubtype TAny _ = True
isSubtype (TArray out) (TArray inn) = isSubtype out inn
isSubtype outter inner = inner == outter

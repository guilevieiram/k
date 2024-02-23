module Interpreter (interpret) where

import Control.Monad.State
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Foldable (find)

import Monads
import ScopedStack
import States
import SystemFunctions
import Tokens
import Utils (replaceNth)

data ExecutionError
    = ExecutionError Source
    | VariableNotFound Source String
    | OperationError Source String
    | EvaluationError Source String
    | IndexError Source String
    deriving (Show, Eq)

-- entrypoint
interpret :: Token -> IO (ExecutionState ExecutionError)
interpret = (`execStateT` defaultState) . runMaybeT . eval
  where
    fStack = [(fName sf, fToken sf) | sf <- sysFunctions]
    defaultState =
        ExecutionState
            { variables = []
            , declaredFunctions = stackInsertMany fStack []
            , executingFunctions = []
            , executionError = Nothing
            , returnValue = VNil
            }

-- implementation
eval :: Token -> MaybeStateIO (ExecutionState ExecutionError) Variable
-- declarations
eval (Declare _ t name) = do
    eState <- liftFromState get
    let varStack = variables eState
    liftFromState $
        put eState{variables = stackInsert name (getDefaultValue t) varStack}
    liftFromVariable VNil

-- vars
eval (Var src x as) = do
    eState <- liftFromState get
    case stackGet x (variables eState) of
        Nothing -> do
            let errMsg = "Variable " ++ x ++ " is not defined."
            put eState{executionError = Just $ VariableNotFound src errMsg}
            liftFromMaybe Nothing
        Just val -> getVar as val
  where
    getVar (e : es) (VArray _ vals) = do
        eVal <- eval e
        case eVal of
            VInt i ->
                if i < length vals
                    then getVar es (vals !! i)
                    else liftFromMaybe Nothing
            _ -> liftFromMaybe Nothing
    getVar [] v = liftFromVariable v
    getVar _ _ = liftFromMaybe Nothing

-- assigning
eval (Assign src (Var _ name []) expr) = do
    eValue <- eval expr
    eState <- liftFromState get
    let varStack = variables eState
    case stackUpdate name eValue varStack of
        Just newMap -> do
            put eState{variables = newMap}
            liftFromVariable VNil
        Nothing -> do
            let errMsg = "Variable name " ++ name ++ " is not defined in this scope."
            put eState{executionError = Just $ VariableNotFound src errMsg}
            MaybeT $ return Nothing
eval (Assign src (Var _ name as) expr) = do
    eValue <- eval expr
    eState <- liftFromState get
    let varStack = variables eState
    let arrVar = stackGet name varStack
    case arrVar of
        Nothing -> do
            let errMsg = "Variable name " ++ name ++ " is not defined in this scope."
            put eState{executionError = Just $ VariableNotFound src errMsg}
            MaybeT $ return Nothing
        Just arr -> replace eValue as arr
  where
    replace val (e : es) (VArray arrSrc ar) = do
        eVal <- eval e
        case eVal of
            VInt i -> do
                eState <- liftFromState get
                new <- replace val es (ar !! i)
                case replaceNth i new ar of
                    Nothing -> do
                        let errMsg = "Indexing out of bounds ."
                        put eState{executionError = Just $ IndexError src errMsg}
                        liftFromMaybe Nothing
                    Just newArray -> updateVar name (VArray arrSrc newArray) eState
            _ -> liftFromMaybe Nothing
    replace val [] _ = liftFromVariable val
    replace _ _ _ = liftFromMaybe Nothing
    updateVar varName val eState = do
        let varStack = variables eState
        case stackUpdate varName val varStack of
            Just newMap -> do
                put eState{variables = newMap}
                liftFromVariable VNil
            Nothing -> do
                let errMsg = "Variable name " ++ varName ++ " is not defined in this scope."
                put eState{executionError = Just $ VariableNotFound src errMsg}
                MaybeT $ return Nothing

-- values
eval (IntValue _ x) = liftFromVariable $ VInt x
eval (FloatValue _ x) = liftFromVariable $ VFloat x
eval (BoolValue _ x) = liftFromVariable $ VBool x
eval (StringValue _ x) = liftFromVariable $ VString x
eval (CharValue _ x) = liftFromVariable $ VChar x
eval (NilValue _) = liftFromVariable VNil
-- expressions, statements and sequences
eval (Expr _ e) = eval e
eval (Stmt _ e) = eval e
eval (Sequence _ []) = liftFromVariable VNil
eval (Sequence src (e : es)) = do
    _ <- eval e
    eval $ Sequence src es

-- operations
eval (UnOp src o x) = do
    xVal <- eval x
    compute o xVal
  where
    compute Neg (VInt val) = liftFromVariable $ VInt (-val)
    compute Neg (VFloat val) = liftFromVariable $ VFloat (-val)
    compute Not (VBool val) = liftFromVariable $ VBool (not val)
    compute _ _ = do
        eState <- liftFromState get
        let msg = "operation error with operand " ++ show o
        put $ eState{executionError = Just $ OperationError src msg}
        liftFromMaybe Nothing
eval (BinOp src o x y) = do
    xVal <- eval x
    yVal <- eval y
    compute o xVal yVal
  where
    compute Plus (VInt xval) (VInt yval) = liftFromVariable . VInt $ xval + yval
    compute Minus (VInt xval) (VInt yval) = liftFromVariable . VInt $ xval - yval
    compute Times (VInt xval) (VInt yval) = liftFromVariable . VInt $ xval * yval
    compute Divide (VInt xval) (VInt yval) = liftFromVariable . VInt $ xval `div` yval
    compute Plus (VFloat xval) (VFloat yval) = liftFromVariable . VFloat $ xval + yval
    compute Minus (VFloat xval) (VFloat yval) = liftFromVariable . VFloat $ xval - yval
    compute Times (VFloat xval) (VFloat yval) = liftFromVariable . VFloat $ xval * yval
    compute Divide (VFloat xval) (VFloat yval) = liftFromVariable . VFloat $ xval / yval
    compute Gt (VInt xval) (VInt yval) = liftFromVariable . VBool $ xval > yval
    compute Ge (VInt xval) (VInt yval) = liftFromVariable . VBool $ xval >= yval
    compute Lt (VInt xval) (VInt yval) = liftFromVariable . VBool $ xval < yval
    compute Le (VInt xval) (VInt yval) = liftFromVariable . VBool $ xval <= yval
    compute Gt (VFloat xval) (VFloat yval) = liftFromVariable . VBool $ xval > yval
    compute Ge (VFloat xval) (VFloat yval) = liftFromVariable . VBool $ xval >= yval
    compute Lt (VFloat xval) (VFloat yval) = liftFromVariable . VBool $ xval < yval
    compute Le (VFloat xval) (VFloat yval) = liftFromVariable . VBool $ xval <= yval
    compute Eq v1 v2 = liftFromVariable . VBool $ v1 == v2
    compute Neq v1 v2 = liftFromVariable . VBool $ v1 /= v2
    compute AndOp (VBool b1) (VBool b2) = liftFromVariable . VBool $ b1 && b2
    compute OrOp (VBool b1) (VBool b2) = liftFromVariable . VBool $ b1 || b2
    compute Concat (VString xs) (VString ys) = liftFromVariable $ VString (xs ++ ys)
    compute Concat (VArray tx xs) (VArray ty ys) =
        if tx == ty
            then liftFromVariable $ VArray tx (xs ++ ys)
            else do
                eState <- liftFromState get
                let msg = "Concatting lists of wrong inner type"
                put $ eState{executionError = Just $ OperationError src msg}
                liftFromMaybe Nothing
    compute _ _ _ = do
        eState <- liftFromState get
        let msg = "operation error with operand " ++ show o
        put $ eState{executionError = Just $ OperationError src msg}
        liftFromMaybe Nothing

-- Conditionals
eval (Conditional src cond tr fl) = eval cond >>= compute
  where
    compute (VBool b) = if b then eval tr else eval fl
    compute _ = do
        eState <- liftFromState get
        let msg = "Condition did not evaluate to boolean"
        put $ eState{executionError = Just $ EvaluationError src msg}
        liftFromMaybe Nothing

-- While loop
eval loop@(WhileLoop src cond branch) = eval cond >>= compute
  where
    compute (VBool b) =
        if b
            then do
                _ <- eval branch
                eval loop
            else liftFromVariable VNil
    compute _ = do
        eState <- liftFromState get
        let msg = "Condition did not evaluate to boolean"
        put $ eState{executionError = Just $ EvaluationError src msg}
        liftFromMaybe Nothing

-- declaring functions
eval function@(Function _ name _ _ _) = do
    eState <- liftFromState get
    let dFuncStack = declaredFunctions eState
    put $ eState{declaredFunctions = stackInsert name function dFuncStack}
    liftFromVariable VNil

-- Calling functions
eval (CallArg _ arg) = eval arg
eval (ReturnVal _ e) = do
    value <- eval e
    eState <- liftFromState get
    liftFromState . put $ eState{returnValue = value}
    liftFromMaybe Nothing
eval (Call _ funcName callArgs@(CallArgs _ cArgs)) = do
    eState <- liftFromState get

    -- get the arguments names and function
    let dFuncStack = declaredFunctions eState
    callingFunction <- liftFromMaybe $ stackGet funcName dFuncStack

    -- checking if sys function is being called
    let mSysF = find ((== funcName) . fName) sysFunctions
    case mSysF of
        Just sysF -> fImplem sysF eval callArgs
        Nothing -> handleFunction eState callingFunction
  where
    handleFunction eState function = do
        argsNames <- liftFromMaybe $ getArgs function

        let varStack = variables eState
        let newVarStack = stackNewScope varStack
        let execFuncsStack = executingFunctions eState
        insertedStack <- insertInStack newVarStack (zip argsNames cArgs)
        liftFromState . put $
            eState
                { executingFunctions = stackInsert funcName function execFuncsStack
                , variables = insertedStack
                }

        body <- liftFromMaybe $ getBody function

        -- capture the return value
        _ <- liftFromState . runMaybeT $ eval body
        -- check state for errors
        newState <- liftFromState get
        let err = executionError newState
        let returnVal = returnValue newState
        case err of
            -- if there is an error just propagate it
            Just _ -> liftFromMaybe Nothing
            -- if no error, we short circuited the return!
            Nothing -> do
                -- drop the function locals and function execution stack
                let afterVarStack = stackDropScope $ variables newState
                afterExecFuncStack <- liftFromMaybe . stackDelete funcName $ executingFunctions newState
                liftFromState . put $
                    eState
                        { executingFunctions = afterExecFuncStack
                        , variables = afterVarStack
                        }

                --  return the return value
                if not $ checkType (getFType function) returnVal
                    then liftFromMaybe Nothing
                    else do
                        liftFromVariable returnVal

    insertInStack :: ScopedStack Variable -> [(String, Token)] -> MaybeStateIO (ExecutionState ExecutionError) (ScopedStack Variable)
    -- insertInStack = undefined
    insertInStack stack [] = return stack
    insertInStack stack ((vname, tok) : rest) = do
        v <- eval tok
        newStack <- insertInStack stack rest
        return $ stackInsert vname v newStack

    getArg (Arg _ _ x) = Just x
    getArg _ = Nothing
    getArgs (Function _ _ _ (Args _ as) _) = mapM getArg as
    getArgs _ = Nothing
    getBody (Function _ _ _ _ body) = Just body
    getBody _ = Nothing
    getFType (Function _ _ fType _ _) = Just fType
    getFType _ = Nothing
    checkType (Just t) var = getType var == t
    checkType Nothing _ = False

-- Default error
eval _ = do
    eState <- liftFromState get
    put eState{executionError = Just $ ExecutionError defaultSource}
    liftFromMaybe Nothing

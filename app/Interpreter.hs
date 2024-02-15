module Interpreter where

import Control.Monad.State
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))

import Source
import Tokens
import Types
import Data.Foldable (find)

interpret :: Token -> IO ExecutionState
interpret = (`execStateT` defaultState) . runMaybeT . eval

eval :: Token -> MaybeExecIO Variable
eval (Declare _ t name) = do
    eState <- liftFromState get
    let varStack = variables eState
    liftFromState . put $ eState{variables = stackInsert name (getDefaultValue t) varStack}
    liftFromVariable VNil
eval (Var src x) = do
    eState <- liftFromState get
    case stackGet x (variables eState) of
        Nothing -> do
            let errMsg = "Variable " ++ x ++ " is not defined."
            put eState{executionError = Just $ UndefinedVariable src errMsg}
            liftFromMaybe Nothing
        Just val -> liftFromVariable val
eval (Assign src name expr) = do
    eValue <- eval expr
    eState <- liftFromState get
    let varStack = variables eState
    case stackUpdate name eValue varStack of
        Just newMap -> do
            put eState{variables = newMap}
            liftFromVariable VNil
        Nothing -> do
            let errMsg = "Variable name " ++ name ++ " is not defined in this scope."
            put eState{executionError = Just $ UndefinedVariable src errMsg}
            MaybeT $ return Nothing
eval (IntValue _ x) = liftFromVariable $ VInt x
eval (FloatValue _ x) = liftFromVariable $ VFloat x
eval (BoolValue _ x) = liftFromVariable $ VBool x
eval (Expr _ e) = eval e
eval (ReturnVal _ e) = do 
    value <- eval e
    eState <- liftFromState get
    liftFromState.put $ eState {returnValue=value}
    liftFromMaybe Nothing
eval (Sequence _ []) = liftFromVariable VNil
eval (Sequence src (e : es)) = do
    _ <- eval e
    eval $ Sequence src es

eval (UnOp src o x) = do
    xVal <- eval x
    compute o xVal
  where
    compute Minus (VInt val) = liftFromVariable $ VInt (-val)
    compute Minus (VFloat val) = liftFromVariable $ VFloat (-val)
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
    compute Eq (VInt xval) (VInt yval) = liftFromVariable . VBool $ xval == yval
    compute Neq (VInt xval) (VInt yval) = liftFromVariable . VBool $ xval /= yval
    compute Gt (VFloat xval) (VFloat yval) = liftFromVariable . VBool $ xval > yval
    compute Ge (VFloat xval) (VFloat yval) = liftFromVariable . VBool $ xval >= yval
    compute Lt (VFloat xval) (VFloat yval) = liftFromVariable . VBool $ xval < yval
    compute Le (VFloat xval) (VFloat yval) = liftFromVariable . VBool $ xval <= yval
    compute Eq (VFloat xval) (VFloat yval) = liftFromVariable . VBool $ xval == yval
    compute Neq (VFloat xval) (VFloat yval) = liftFromVariable . VBool $ xval /= yval
    compute _ _ _ = do
        eState <- liftFromState get
        let msg = "operation error with operand " ++ show o
        put $ eState{executionError = Just $ OperationError src msg}
        liftFromMaybe Nothing
eval (Conditional src cond tr fl) = eval cond >>= compute
  where
    compute (VBool b) = if b then eval tr else eval fl
    compute _ = do
        eState <- liftFromState get
        let msg = "Condition did not evaluate to boolean"
        put $ eState{executionError = Just $ EvaluationError src msg}
        liftFromMaybe Nothing
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
eval function@(Function _ name _ _ _) = do
    eState <- liftFromState get
    let dFuncStack = declaredFunctions eState
    put $ eState{declaredFunctions = stackInsert name function dFuncStack}
    liftFromVariable VNil

eval (Call _ funcName callArgs@(CallArgs _ cArgs)) = do

    eState <- liftFromState get

    -- get the arguments names and function
    let dFuncStack = declaredFunctions eState
    callingFunction <- liftFromMaybe $ stackGet funcName dFuncStack
    
    -- checking if sys function is being called
    let mSysF = find ((==funcName) . fName ) sysFunctions
    case mSysF of 
        Just sysF -> fImplem sysF eval callArgs
        Nothing -> do
            argsNames <- liftFromMaybe $ getArgs callingFunction

            -- update the variable stack with new scope
            let varStack = variables eState
            let newVarStack = stackNewScope varStack
            insertedStack <- insertInStack newVarStack (zip argsNames cArgs)
            
            -- update the executingFunctions stack 
            let execFuncsStack = executingFunctions eState
            liftFromState . put $ eState
                {executingFunctions = stackInsert funcName callingFunction execFuncsStack
                , variables = insertedStack
                }

            -- execute the function
            body <- liftFromMaybe $ getBody callingFunction

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
                    -- drop the function locals and function jexecution stack
                    let afterVarStack = stackDropScope $ variables newState
                    afterExecFuncStack <- liftFromMaybe . stackDelete funcName $ executingFunctions newState
                    liftFromState . put $ eState
                        { executingFunctions = afterExecFuncStack
                        , variables = afterVarStack
                        }
                    --  return the return value
                    liftFromVariable returnVal
  where
    insertInStack :: ScopedStack Variable -> [(String, Token)] -> MaybeExecIO (ScopedStack Variable)
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
    getBody (Function _ _ _  _ body ) = Just body
    getBody _ = Nothing

eval _ = do
    eState <- liftFromState get
    put eState{executionError = Just $ ExecutionError defaultSource}
    liftFromMaybe Nothing

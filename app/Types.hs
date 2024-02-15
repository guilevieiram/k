module Types where

import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Functor

import Source
import Tokens

data Variable
    = VInt Int
    | VBool Bool
    | VFloat Float
    | VNil
    deriving (Show, Eq)

getDefaultValue :: Types -> Variable
getDefaultValue TInt = VInt 0
getDefaultValue TFloat = VFloat 0
getDefaultValue TBool = VBool True
getDefaultValue TNil = VNil

data ExecutionError
    = ExecutionError Source
    | UndefinedVariable Source String
    | OperationError Source String
    | EvaluationError Source String
    deriving (Show, Eq)

type ScopedStack a = [[(String, a)]]

type ExecT = StateT ExecutionState
type MaybeExecIO = MaybeT (ExecT IO)

stackNewScope :: ScopedStack a -> ScopedStack a
stackNewScope = ([] :)
stackDropScope :: ScopedStack a -> ScopedStack a
stackDropScope = drop 1

stackGet :: String -> ScopedStack a -> Maybe a
stackGet _ [] = Nothing
stackGet name (scope : rest) =
    case lookup name scope of
        Just var -> Just var
        Nothing -> stackGet name rest

stackInsert :: String -> a -> ScopedStack a -> ScopedStack a
stackInsert name var [] = [[(name, var)]]
stackInsert name var (scope : rest) = ((name, var) : scope) : rest

stackInsertMany :: [(String, a)] -> ScopedStack a -> ScopedStack a
stackInsertMany [] stack = stack
stackInsertMany ((name, var) : rest) stack = stackInsertMany rest $ stackInsert name var stack

stackUpdate :: String -> a -> ScopedStack a -> Maybe (ScopedStack a)
stackUpdate _ _ [] = Nothing
stackUpdate name var (scope : rest) =
    if any ((== name) . fst) scope
        then return $ map updating scope : rest
        else fmap (scope :) (stackUpdate name var rest)
  where
    updating t@(n, _) = if n == name then (n, var) else t

stackDelete :: (Eq a) => String -> ScopedStack a -> Maybe (ScopedStack a)
stackDelete _ [] = Nothing
stackDelete name (scope : rest) =
    let deleted = filter ((/= name) . fst) scope
     in if deleted == scope
            then stackDelete name rest <&> (scope :)
            else return $ deleted : rest

data ExecutionState = ExecutionState
    { variables :: ScopedStack Variable
    , declaredFunctions :: ScopedStack Token
    , executingFunctions :: ScopedStack Token
    , returnValue :: Variable
    , executionError :: Maybe ExecutionError
    }
    deriving (Show, Eq)

defaultState :: ExecutionState
defaultState =
    ExecutionState
        { variables = []
        , declaredFunctions = stackInsertMany fStack []
        , executingFunctions = []
        , executionError = Nothing
        , returnValue = VNil
        }
  where
    fStack = [(fName sf, fToken sf) | sf <- sysFunctions]

liftFromState :: (Monad m) => m a -> MaybeT m a
liftFromState = lift

liftFromVariable :: a -> MaybeExecIO a
liftFromVariable = liftFromState . lift . returnIO

liftFromIO :: IO a -> MaybeExecIO a
liftFromIO = liftFromState . lift

liftFromMaybe :: (Monad m) => Maybe a -> MaybeT m a
liftFromMaybe = MaybeT . return

returnIO :: a -> IO a
returnIO = return

-- sys functions
type SysFunctionImplementation =
    (Token -> MaybeExecIO Variable) ->
    Token ->
    MaybeExecIO Variable

sysPrint :: (Token -> MaybeExecIO Variable) -> Token -> MaybeExecIO Variable
sysPrint exec (CallArgs _ as)
    | length as /= 1 = liftFromMaybe Nothing
    | otherwise = do
        let arg = head as
        aVal <- exec arg
        case aVal of
            VInt x -> liftFromIO . putStr $ show x
            VFloat x -> liftFromIO . putStr $ show x
            VBool x -> liftFromIO . putStr $ show x
            VNil -> liftFromIO . putStr $ "NIL"
        liftFromVariable VNil
sysPrint _ _ = liftFromMaybe Nothing

sysRead :: (String -> Variable) -> (Token -> MaybeExecIO Variable) -> Token -> MaybeExecIO Variable
sysRead reader _ (CallArgs _ as)
    | not (null as) = liftFromMaybe Nothing
    | otherwise =
        liftFromIO getLine
            >>= liftFromVariable . reader
sysRead _ _ _ = liftFromMaybe Nothing

-- listing the possible functions
data SysFunction = SysFunction
    { fName :: String
    , fToken :: Token
    , fImplem :: SysFunctionImplementation
    }

sysFunctions :: [SysFunction]
sysFunctions =
    [ SysFunction
        { fName = name
        , fToken =
            Function
                defaultSource
                name
                TNil
                (Args defaultSource [Arg defaultSource t ""])
                (NilValue defaultSource)
        , fImplem = sysPrint
        }
    | (name, t) <-
        [ ("print_int", TInt)
        , ("print_float", TFloat)
        , ("print_bool", TBool)
        ]
    ] ++ 
    [SysFunction 
        {fName = name
        , fImplem = sysRead reader
        , fToken = Function 
            defaultSource 
            name
            t
            (Args defaultSource [])
            (NilValue defaultSource)
        }
        | (name, reader, t) <- [
            ("read_int", VInt . read , TInt),
            ("read_float", VFloat .read, TFloat)
        ]
    ]

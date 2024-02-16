module SystemFunctions where

import Monads
import States
import Tokens

type SysFunctionImplementation e =
    (Token -> MaybeStateIO (ExecutionState e) Variable) ->
    Token ->
    MaybeStateIO (ExecutionState e) Variable

data SysFunction e = SysFunction
    { fName :: String
    , fToken :: Token
    , fImplem :: SysFunctionImplementation e
    }

sysFunctions :: [SysFunction e]
sysFunctions = sysPrints ++ sysReads

sysPrint :: (Token -> MaybeStateIO (ExecutionState e) Variable) -> Token -> MaybeStateIO (ExecutionState e) Variable
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

sysRead :: (String -> Variable) -> (Token -> MaybeStateIO (ExecutionState e) Variable) -> Token -> MaybeStateIO (ExecutionState e) Variable
sysRead reader _ (CallArgs _ as)
    | not (null as) = liftFromMaybe Nothing
    | otherwise =
        liftFromIO getLine
            >>= liftFromVariable . reader
sysRead _ _ _ = liftFromMaybe Nothing

-- listing the possible functions
sysPrints :: [SysFunction e]
sysPrints =
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
    ]

sysReads :: [SysFunction e]
sysReads =
    [ SysFunction
        { fName = name
        , fImplem = sysRead reader
        , fToken =
            Function
                defaultSource
                name
                t
                (Args defaultSource [])
                (NilValue defaultSource)
        }
    | (name, reader, t) <-
        [ ("read_int", VInt . read, TInt)
        , ("read_float", VFloat . read, TFloat)
        ]
    ]

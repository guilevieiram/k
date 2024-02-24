module SystemFunctions where

import Control.Monad.State
import Data.List
import System.IO (hFlush, stdout)

import Monads
import ScopedStack
import States
import Tokens
import Utils

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
sysFunctions = sysIos ++ sysGetTypes ++ sysArrays ++ sysConverts

-- listing the possible functions
sysConverts :: [SysFunction e]
sysConverts =
    [ SysFunction
        { fName = name
        , fImplem = imp
        , fToken =
            Function
                defaultSource
                name
                tOut
                ( Args
                    defaultSource
                    [Arg defaultSource tIn ""]
                )
                (NilValue defaultSource)
        }
    | (imp, tIn, tOut, name) <-
        [ (sysInt2Str, TInt, TString, "int2str")
        , (sysChar2Str, TChar, TString, "char2str")
        , (sysFloat2Str, TFloat, TString, "float2str")
        , (sysBool2Str, TBool, TString, "bool2str")
        , (sysNil2Str, TNil, TString, "nil2str")
        , (sysStr2Int, TString, TInt, "str2int")
        , (sysStr2Float, TString, TFloat, "str2float")
        , (sysStr2Bool, TString, TBool, "str2bool")
        , (sysStr2Nil, TString, TNil, "str2nil")
        , (sysStr2CharArray, TString, TArray TChar, "str2chars")
        ]
    ]
sysArrays :: [SysFunction e]
sysArrays =
    [ SysFunction
        { fName = "make_array"
        , fImplem = sysMakeArray
        , fToken =
            Function
                defaultSource
                "make_array"
                TNil
                ( Args
                    defaultSource
                    [ Arg defaultSource (TArray TAny) ""
                    , Arg defaultSource TInt ""
                    ]
                )
                (NilValue defaultSource)
        }
    , SysFunction
        { fName = "len"
        , fImplem = sysLenArray
        , fToken =
            Function
                defaultSource
                "len"
                TInt
                ( Args
                    defaultSource
                    [ Arg defaultSource (TArray TAny) ""
                    ]
                )
                (NilValue defaultSource)
        }
    ]
sysGetTypes :: [SysFunction e]
sysGetTypes =
    [ SysFunction
        { fName = "typeof"
        , fToken =
            Function
                defaultSource
                "typeof"
                TString
                (Args defaultSource [Arg defaultSource TAny ""])
                (NilValue defaultSource)
        , fImplem = sysGetType
        }
    ]
sysIos :: [SysFunction e]
sysIos =
    [ SysFunction
        { fName = "print"
        , fImplem = sysPrint
        , fToken =
            Function
                defaultSource
                "print"
                TNil
                (Args defaultSource [Arg defaultSource TString ""])
                (NilValue defaultSource)
        }
    , SysFunction
        { fName = "read"
        , fImplem = sysRead
        , fToken =
            Function
                defaultSource
                "read"
                TString
                (Args defaultSource [])
                (NilValue defaultSource)
        }
    ]

-- implementations
sysLenArray :: SysFunctionImplementation e
sysLenArray
    exec
    ( CallArgs
            _
            [ CallArg _ expr
                ]
        ) = do
        e <- exec expr
        case e of
            VArray _ arr -> liftFromVariable . VInt $ length arr
            _ -> liftFromMaybe Nothing
sysLenArray _ _ = liftFromMaybe Nothing

sysMakeArray :: SysFunctionImplementation e
sysMakeArray
    exec
    ( CallArgs
            _
            [ CallArg _ (Var _ name as)
                , CallArg _ len
                ]
        ) = do
        lenVar <- exec len
        eState <- liftFromState get
        let varStack = variables eState
        let arrVar = stackGet name varStack
        case (arrVar, lenVar) of
            (Just a@(VArray t _), VInt l) ->
                if null as
                    then do
                        newArr <- replace as a t l
                        updateVar name newArr eState
                    else replace as a t l
            (_, _) -> liftFromMaybe Nothing
      where
        replace (e : es) (VArray arrSrc ar) (TArray t) l = do
            eVal <- exec e
            case eVal of
                VInt i -> do
                    eState <- liftFromState get
                    new <- replace es (ar !! i) t l
                    case replaceNth i new ar of
                        Nothing -> liftFromMaybe Nothing
                        Just newArray -> updateVar name (VArray arrSrc newArray) eState
                _ -> liftFromMaybe Nothing
        replace [] _ t l = liftFromVariable (VArray t [getDefaultValue t | _ <- [1 .. l]])
        replace _ _ _ _ = liftFromMaybe Nothing
        updateVar varName val eState = do
            let varStack = variables eState
            case stackUpdate varName val varStack of
                Just newMap -> do
                    put eState{variables = newMap}
                    liftFromVariable val
                Nothing -> liftFromMaybe Nothing
sysMakeArray _ _ = liftFromMaybe Nothing

sysGetType :: SysFunctionImplementation e
sysGetType exec (CallArgs _ as)
    | length as /= 1 = liftFromMaybe Nothing
    | otherwise = do
        val <- exec $ head as
        let valType = getType val
        liftFromVariable $ VString (convert valType)
  where
    convert TInt = "int"
    convert TFloat = "float"
    convert TBool = "bool"
    convert TNil = "nil"
    convert TString = "str"
    convert TChar = "char"
    convert TAny = "any"
    convert (TArray t) = "array<" ++ convert t ++ ">"
sysGetType _ _ = liftFromMaybe Nothing

sysPrint ::
    (Token -> MaybeStateIO (ExecutionState e) Variable) ->
    Token ->
    MaybeStateIO (ExecutionState e) Variable
sysPrint exec (CallArgs _ as)
    | length as /= 1 = liftFromMaybe Nothing
    | otherwise = do
        let arg = head as
        aVal <- exec arg
        _ <- liftFromIO . putStr . strVar $ aVal
        liftFromIO $ hFlush stdout
        liftFromVariable VNil
  where
    strVar v = case v of
        VInt x -> show x
        VFloat x -> show x
        VBool x -> show x
        VString x -> x
        VChar x -> [x]
        VNil -> "NIL"
        VArray _ els -> "[" ++ intercalate ", " (map strVar els) ++ "]"
sysPrint _ _ = liftFromMaybe Nothing

sysRead :: SysFunctionImplementation e
sysRead _ (CallArgs _ as)
    | not (null as) = liftFromMaybe Nothing
    | otherwise =
        liftFromIO getLine
            >>= liftFromVariable . VString
sysRead _ _ = liftFromMaybe Nothing

sysInt2Str :: SysFunctionImplementation e
sysInt2Str exec (CallArgs _ [CallArg _ tok]) = do
    e <- exec tok
    case e of
        VInt i -> liftFromVariable . VString $ show i
        _ -> liftFromMaybe Nothing
sysInt2Str _ _ = liftFromMaybe Nothing
sysChar2Str :: SysFunctionImplementation e
sysChar2Str exec (CallArgs _ [CallArg _ tok]) = do
    e <- exec tok
    case e of
        VChar i -> liftFromVariable . VString $ [i]
        _ -> liftFromMaybe Nothing
sysChar2Str _ _ = liftFromMaybe Nothing
sysFloat2Str :: SysFunctionImplementation e
sysFloat2Str exec (CallArgs _ [CallArg _ tok]) = do
    e <- exec tok
    case e of
        VFloat i -> liftFromVariable . VString $ show i
        _ -> liftFromMaybe Nothing
sysFloat2Str _ _ = liftFromMaybe Nothing
sysBool2Str :: SysFunctionImplementation e
sysBool2Str exec (CallArgs _ [CallArg _ tok]) = do
    e <- exec tok
    case e of
        VBool i -> liftFromVariable . VString $ show i
        _ -> liftFromMaybe Nothing
sysBool2Str _ _ = liftFromMaybe Nothing
sysNil2Str :: SysFunctionImplementation e
sysNil2Str exec (CallArgs _ [CallArg _ tok]) = do
    e <- exec tok
    case e of
        VNil -> liftFromVariable . VString $ "NIL"
        _ -> liftFromMaybe Nothing
sysNil2Str _ _ = liftFromMaybe Nothing
sysStr2Int :: SysFunctionImplementation e
sysStr2Int exec (CallArgs _ [CallArg _ tok]) = do
    e <- exec tok
    case e of
        VString x -> liftFromVariable . VInt $ read x
        _ -> liftFromMaybe Nothing
sysStr2Int _ _ = liftFromMaybe Nothing
sysStr2Float :: SysFunctionImplementation e
sysStr2Float exec (CallArgs _ [CallArg _ tok]) = do
    e <- exec tok
    case e of
        VString x -> liftFromVariable . VFloat $ read x
        _ -> liftFromMaybe Nothing
sysStr2Float _ _ = liftFromMaybe Nothing
sysStr2Bool :: SysFunctionImplementation e
sysStr2Bool exec (CallArgs _ [CallArg _ tok]) = do
    e <- exec tok
    case e of
        VString x -> liftFromVariable . VBool $ read x
        _ -> liftFromMaybe Nothing
sysStr2Bool _ _ = liftFromMaybe Nothing
sysStr2Nil :: SysFunctionImplementation e
sysStr2Nil exec (CallArgs _ [CallArg _ tok]) = do
    e <- exec tok
    case e of
        VString "NIL" -> liftFromVariable VNil
        _ -> liftFromMaybe Nothing
sysStr2Nil _ _ = liftFromMaybe Nothing

sysStr2CharArray :: SysFunctionImplementation e
sysStr2CharArray exec (CallArgs _ [CallArg _ tok]) = do
    e <- exec tok
    case e of
        VString str -> liftFromVariable . VArray TChar $ map VChar str
        _ -> liftFromMaybe Nothing
sysStr2CharArray _ _ = liftFromMaybe Nothing

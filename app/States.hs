module States where

import ScopedStack
import Tokens

-- simple state stacks
type VariablesStack = [(String, Types)]
type FunctionStack = [Token]

data StateStack = StateStack
    { vars :: VariablesStack
    , funcs :: FunctionStack
    , defFuncs :: FunctionStack
    }

data ExecutionState e = ExecutionState
    { variables :: ScopedStack Variable
    , declaredFunctions :: ScopedStack Token
    , executingFunctions :: ScopedStack Token
    , returnValue :: Variable
    , executionError :: Maybe e
    }
    deriving (Show, Eq)

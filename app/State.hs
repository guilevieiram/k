module State where

data State = State
    { line :: Int
    , col :: Int
    , file :: String
    }
    deriving (Show, Eq)

defaultState :: State
defaultState =
    State
        { line = 0
        , col = 0
        , file = ""
        }

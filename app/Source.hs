module Source where

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

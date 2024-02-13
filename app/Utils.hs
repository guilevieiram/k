module Utils where

-- monad for error handling when assembling pipeline
(->>) :: Either e a -> (e -> f) -> Either f a
action ->> handler =
    case action of
        Left err -> Left . handler $ err
        Right result -> Right result

module Utils where

-- monad for error handling when assembling pipeline
(->>) :: Either e a -> (e -> f) -> Either f a
action ->> handler =
    case action of
        Left err -> Left . handler $ err
        Right result -> Right result

dropWhileList :: ([a] -> Bool) -> [a] -> [a]
dropWhileList _ [] = []
dropWhileList predicate lst
    | predicate lst = dropWhileList predicate (tail lst)
    | otherwise = lst

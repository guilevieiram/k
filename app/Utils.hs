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

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

processEscapes :: String -> String
processEscapes [] = []
processEscapes ('\\' : 'n' : xs) = '\n' : processEscapes xs
processEscapes ('\\' : 't' : xs) = '\t' : processEscapes xs
processEscapes (x : xs) = x : processEscapes xs

replaceNth :: Int -> a -> [a] -> Maybe [a]
replaceNth n el list
    | n < length list = Just $ take n list ++ [el] ++ drop (n + 1) list
    | otherwise = Nothing

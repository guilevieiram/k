module ScopedStack where

import Data.Foldable (find)

type ScopedStack a = [[(String, a)]]

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

stackGetFromVal :: (Eq a) => a -> ScopedStack a -> Maybe String
stackGetFromVal _ [] = Nothing
stackGetFromVal var (scope : rest) =
    case lookup var (map rev scope)of
        Just name -> Just name 
        Nothing -> stackGetFromVal var rest
    where rev (x, y) = (y, x)

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
stackDelete name (scope : rest) = case found of
    Nothing -> (scope :) <$> stackDelete name rest
    Just el -> Just $ removeFirst (== el) scope : rest
  where
    found = find ((== name) . fst) scope
    removeFirst _ [] = []
    removeFirst p (x : xs)
        | p x = xs
        | otherwise = x : removeFirst p xs

module Lib
    ( someFunc
    ) where

import Control.Applicative

someFunc :: IO ()
someFunc = do
  putStrLn "someFunc"


data BinTree a = Node a (BinTree a) (BinTree a) | Nil deriving (Eq, Show)

lca :: Ord a => a -> a -> BinTree a -> Maybe a
lca _ _ Nil = Nothing
lca m n (Node v l r) | has m (Node v l Nil) && has n (Node v Nil r) = Just v
                         | otherwise = lca m n l <|> lca m n r

has :: Eq a => a -> BinTree a -> Bool
has _ Nil = False
has e (Node v l r) = v == e || has e l || has e r

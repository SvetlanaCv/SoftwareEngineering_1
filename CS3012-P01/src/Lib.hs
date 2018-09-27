module Lib
    ( someFunc, BinTree(Node, Nil), lca, tip
    ) where

import Control.Applicative

data BinTree a = Node a (BinTree a) (BinTree a) | Nil deriving (Eq, Show)

lca :: Ord a => a -> a -> BinTree a -> Maybe a
lca _ _ Nil = Nothing
lca m n (Node v l r) | has m (Node v l Nil) && has n (Node v Nil r) = Just v
                         | otherwise = lca m n l <|> lca m n r

has :: Eq a => a -> BinTree a -> Bool
has _ Nil = False
has e (Node v l r) = v == e || has e l || has e r

tip n = Node n Nil Nil

someFunc :: IO()
someFunc = do let tree = Node 8 (Node 3 (tip 1) (Node 6 (tip 4) (tip 7)))
                                (Node 10 Nil (Node 14 (tip 13) Nil))
              print $ lca 4  7 tree
              print $ lca 4 10 tree
              print $ lca 1  4 tree
              print $ lca 1  3 tree
              print $ lca 3  6 tree
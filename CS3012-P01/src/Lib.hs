module Lib
    ( lca, Path, someFunc
    ) where

type Id = Int
data Path = [Id] :# !Int

a :: Path
a = [4,2,1] :# 5
b :: Path
b = [5,3,1] :# 3

someFunc :: IO()
someFunc = do
      print x where ((x:xs) :# n) = (lca a b)

empty :: Path
empty = [] :# 0

cons :: Id -> Path -> Path            --adds val to head of list
cons a (ys :# n) = (a:ys) :# (n + 1)

lca :: Path -> Path -> Path
lca (xs0 :# i) (ys0 :# j) = go k (drop (i-k) xs0) (drop (j-k) ys0) where
  k = min i j
  go n xxs@(x:xs) (y:ys) = case(x) of
                              (y) -> xxs :# n
                              _   -> go (n-1) xs ys

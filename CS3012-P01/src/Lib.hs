module Lib
    ( someFunc, lca, Path, list
    ) where

type Id = Int

data Path = [Id] :# !Int

empty :: Path
empty = [] :# 0

list :: [Id] -> Int -> Path
list a b = Path a b

cons :: Id -> Path -> Path
cons a (ys :# n) = (a:ys) :# (n + 1)

lca :: Path -> Path -> Path
lca (xs0 :# i) (ys0 :# j) = go k (drop (i-k) xs0) (drop (j-k) ys0) where
  k = min i j
  go !n xxs@(x:xs) (y:ys) =
    x == y   = xxs :# n
    | otherwse = go (n - 1) xs ys

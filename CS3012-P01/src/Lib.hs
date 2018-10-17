module Lib
    ( lca, Path(..), someFunc, pathList, Id, lcaToString
    ) where

type Id = Int
data Path = [Id] :# !Int

pathList :: [Id] -> Int -> Path
pathList a b = a :# b

a = pathList [7,6,3,8,9] 5
b = pathList [10,8,9] 3

someFunc :: IO()
someFunc = do
      print (lcaToString (lca a b))                      --x where ((x:xs) :# n) = (lca a b)

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

lcaToString :: Path -> [Int]
lcaToString ((x:xs) :# n) = (x:xs)

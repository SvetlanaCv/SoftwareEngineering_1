module Lib
    ( daglca, Path(..), someFunc, pathList, Id, lcaToString
    ) where

type Id = Int
data Path = [Id] :# !Int

pathList :: [Id] -> Int -> Path
pathList a b = a :# b

a = pathList [7,6,3,8,9] 5
b = pathList [10,8,9] 3

someFunc :: IO()
someFunc = do
      print (lcaToString (daglca a b))                      --x where ((x:xs) :# n) = (lca a b)

empty :: Path
empty = [] :# 0

cons :: Id -> Path -> Path            --adds val to head of list
cons a (ys :# n) = (a:ys) :# (n + 1)

daglca :: Path -> Path -> Path
daglca (xs0 :# i) (ys0 :# j) = if(xs0 == [] || ys0 == []) then empty else go k (drop (i-k) xs0) (drop (j-k) ys0) where
  k = min i j
  go n xxs@(x:xs) (y:ys) = if (x==y) then xxs :# n else go (n-1) xs ys

lcaToString :: Path -> [Int]
lcaToString ((x:xs) :# n) = (x:xs)
lcaToString empty = []

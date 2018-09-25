module Spec (spec) where

import           Lib
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec :: Spec
spec = do
  describe "LCA Test" $ do

    it "returns Just 0 for 0/x" $ do
    do let tip n = Node n Nil Nil
       let tree = Node 8 (Node 3 (tip 1) (Node 6 (tip 4) (tip 7)))
                         (Node 10 Nil (Node 14 (tip 13) Nil))
       print $ lca 4  7 tree
       print $ lca 4 10 tree
       print $ lca 1  4 tree
       print $ lca 1  3 tree
       print $ lca 3  6 tree

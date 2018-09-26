module Spec (spec) where

import           Lib
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "LCA Test" $ do

    it "Test1 for tree 1" $ do
      (lca 4 7 tree) == Just 6 `shouldBe` True

    it "Test2 tree 1" $ do
      (lca 4 10 tree) == Just 8 `shouldBe` True

    it "Tests tree 1" $ do
      (lca 1 3 tree) == Just 3 `shouldBe` True

    it "Tests tree 1" $ do
      (lca 1 3 tree) == Just 3 `shouldBe` True

    it "Tests tree 1" $ do
      (lca 3 6 tree) == Just 3 `shouldBe` True

tip n = Node n Nil Nil
tree = Node 8 (Node 3 (tip 1) (Node 6 (tip 4) (tip 7)))
              (Node 10 Nil (Node 14 (tip 13) Nil))

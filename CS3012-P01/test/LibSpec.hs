module LibSpec (spec) where

import           Lib
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "LCA Test1" $ do

    it "Test1 for tree 1" $ do
      (lca 4 7 tree) == Just 6 `shouldBe` True

    it "Test2 tree 1" $ do
      (lca 4 10 tree) == Just 8 `shouldBe` True

    it "Test3 tree 1" $ do
      (lca 1 3 tree) == Just 3 `shouldBe` True

    it "Test4 tree 1" $ do
      (lca 1 3 tree) == Just 3 `shouldBe` True

    it "Test5 tree 1" $ do
      (lca 3 6 tree) == Just 3 `shouldBe` True

    it "Test1 for tree 2" $ do
      (lca 4 7 tree2) == Just 1 `shouldBe` True

    it "Test2 tree 2" $ do
      (lca 2 3 tree2) == Just 1 `shouldBe` True

    it "Test3 tree 2" $ do
      (lca 5 3 tree2) == Just 1 `shouldBe` True

    it "Test4 tree 2" $ do
      (lca 2 6 tree2) == Just 1 `shouldBe` True

    it "Test5 tree 2" $ do
      (lca 1 1 tree2) == Just 1 `shouldBe` True

    it "Test5 tree 2" $ do
      (lca 4 5 tree2) == Just 2 `shouldBe` True

tree = Node 8 (Node 3 (tip 1) (Node 6 (tip 4) (tip 7)))
              (Node 10 Nil (Node 14 (tip 13) Nil))
tree2 = Node 1 (Node 2 (tip 4)(tip 5))
               (Node 3 (tip 6)(tip 7))
module LibSpec (spec) where

import           Lib
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "LCA DAG Tests" $ do

    it "Test 1 for tree 1" $ do
      (lca 4 7 tree4) == Just 6 `shouldBe` True

    it "Test 2 tree 1" $ do
      (lca 4 10 tree4) == Just 8 `shouldBe` True

    it "Test 3 tree 1" $ do
      (lca 1 3 tree4) == Just 3 `shouldBe` True

    it "Test 4 tree 1" $ do
      (lca 1 3 tree4) == Just 3 `shouldBe` True

    it "Test 5 tree 1" $ do
      (lca 3 6 tree4) == Just 3 `shouldBe` True

a = list [3,2,1] 3
b = list [5,4,2,1] 4

module LibSpec (spec) where

import           Lib
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "LCA DAG Tests" $ do

    it "Test 1 for tree 1" $ do
       lcaToString ((lca a b)) `shouldBe` [8,9]

  --  it "Test 2 tree 1" $ do
    --  (lca 4 10 tree4) == Just 8 `shouldBe` True

  --  it "Test 3 tree 1" $ do
    --  (lca 1 3 tree4) == Just 3 `shouldBe` True

  --  it "Test 4 tree 1" $ do
  --    (lca 1 3 tree4) == Just 3 `shouldBe` True

  --  it "Test 5 tree 1" $ do
  --    (lca 3 6 tree4) == Just 3 `shouldBe` True

a :: Path
a =  [7,6,3,8,9] :# 5

b :: Path
b =  [10,8,9] :# 3

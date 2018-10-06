module LibSpec (spec) where

import           Lib
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "LCA DAG Tests" $ do

    it "Test 1" $ do
      (lca 2 3 tree) == Just 1 `shouldBe` True

    it "Test 2" $ do
      (lca 4 5 tree2) == Just 1 `shouldBe` True

    it "Test 3" $ do
      (lca 4 5 tree3) == Just 4 `shouldBe` True

    it "Test 4" $ do
      (lca 3 6 tree3) == Just 2 `shouldBe` True

    it "Test 5" $ do
      (lca 6 6 tree3) == Just 6 `shouldBe` True

    it "Test 6" $ do
      (lca 100 1 tree3) == Nothing `shouldBe` True

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

first n = Node 1 [] False False 0
sec n = Node 2 [first] False False 0
third n = Node 3 [first] False False 0  ;node definition
fourth n = Node 4 [sec] False False 0
fifth n = Node 5 [third] False False 0

first2 n = Node 1 [] False False 0
sec2 n = Node 2 [first2] False False 0
third2 n = Node 3 [sec2] False False 0
fourth2 n = Node 4 [third2] False False 0
seventh2 n = Node 7 [sec2] False False 0
sixth2 n = Node 6 [seventh2] False False 0
fifth2 n = Node 5 [fourth3, sixth2] False False 0

eighth3 n = Node 8 [] False False 0
third3 n = Node 3 [eighth3] False False 0
first3 n = Node 1 [third3] False False 0
sixth3 n = Node 6 [third3] False False 0
fourth3 n = Node 4 [sixth3] False False 0
seventh3 n = Node 7 [sixth3] False False 0
tenth3 n = Node 10 [eighth3] False False 0
fourteenth3 n = Node 14 [tenth3] False False 0
thirteenth3 n = Node 13 [fourteenth3] False False 0

tree = [first, sec, third]              ;graph setup
tree2 = [first, sec, third, fourth, fifth]
tree3 = [first2, sec2, third2, fourth2, seventh2, sixth2, fifth2]
tree4 = [eighth3, third3, first3, sixth3, fourth3, seventh3, tenth3, fourteenth3, thirteenth3]

module LibSpec (spec) where

import           Lib
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "LCA DAG Tests" $ do

    it "Test 1 for tree 1" $ do
       lcaToString ((daglca first second)) `shouldBe` [8,9]

    it "Test 2 tree 1" $ do
      lcaToString ((daglca a b)) `shouldBe` [3,8]

    it "Test 3 tree 1" $ do
      lcaToString ((daglca f h)) `shouldBe` [8]

    it "Test 4 tree 1" $ do
      lcaToString ((daglca b c)) `shouldBe` [6,3,8]

    it "Test 5 tree 1" $ do
      lcaToString ((daglca i i)) `shouldBe` [13,14,10,8]

    it "Test 5 tree 1" $ do
      lcaToString ((daglca j d)) `shouldBe` []

first = pathList [7,6,3,8,9] 5
second = pathList [10,8,9] 3

a = pathList [1,3,8] 3
b = pathList [4,6,3,8] 4
c = pathList [7,6,3,8] 4
d = pathList [6,3,8] 3
e = pathList [3,8] 2
f = pathList [8] 1
g = pathList [10,8] 2
h = pathList [14,10,8] 3
i = pathList [13,14,10,8] 4
j = pathList [] 0
k = pathList [2,5,9] 3

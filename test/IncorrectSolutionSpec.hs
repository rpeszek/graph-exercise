module IncorrectSolutionSpec (main, spec) where


import Test.Hspec
import Data.Graph.Inductive.Graph (mkUGraph)
import Data.Graph.Inductive.PatriciaTree (UGr)
import Test.QuickCheck
import IncorrectSolution (bftLongestPath)
import TestData

spec :: Spec
spec = do
  describe "longest path using incorrect solution" $ do
    it "stackoverflow test match" $ do
          (bftLongestPath 1 stackoverflowTest) `shouldBe` [7,5,3,1] -- this matches stackoverflow test but the longest path in this graph is [1,3,5,2,6]
    it "bftCounterExample fails" $ do
           (bftLongestPath 1 bftCounterExample) `shouldBe` [3,2,1]  -- fails
    it "fails on arbitrary DAG" $ property $ 
       (\testG  -> 
           case (dagLongestPath testG) of
             [] -> True
             (n:_) -> (length $ dagLongestPath testG) == (length $ bftLongestPath n (toGraph testG))
       )

main :: IO ()
main = hspec spec


{- Property test data generation -}


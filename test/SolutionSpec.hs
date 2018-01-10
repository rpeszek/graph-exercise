module SolutionSpec (main, spec) where


import Test.Hspec
import Data.Graph.Inductive.Graph (mkUGraph)
import Data.Graph.Inductive.PatriciaTree (UGr)
import Test.QuickCheck
import Solution (longestPath)
import TestData


spec :: Spec
spec = do
  describe "longest path using fold of topological sort" $ do
    it "bftCounterExample works" $ do
           (longestPath 1 bftCounterExample) `shouldBe` Just [1,2,3]  
    it "works on arbitrary DAG" $ property $ 
       (\testG  -> 
           case (dagLongestPath testG) of
             [] -> True
             (n:_) -> (Just . length $ dagLongestPath testG) == (fmap length $ longestPath n (toExampleDag testG))
       )

main :: IO ()
main = hspec spec


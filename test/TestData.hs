{-#LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module TestData where

import Test.QuickCheck
import Data.List
import Data.Graph.Inductive.Graph -- (mkUGraph, Graph)
import Data.Graph.Inductive.PatriciaTree (UGr, Gr)
import Data.Graph.Inductive.Query.DFS (topsort)
import System.Random (Random)
import Dag

{- |
ExampleDag contains simple test graphs. DAG (lack of cycles) is not type enforced 
and it is developer responsibility to use it with true DAGs
-}
newtype ExampleDag a b = ExampleDag (Gr a b) deriving Graph  
instance Dag ExampleDag 

{-| Example copied from stack overflow bts solution 
 https://stackoverflow.com/questions/40647260/finding-the-longest-path-through-a-dfs-forest-in-haskell     
-}
stackoverflowTest:: ExampleDag () ()
stackoverflowTest = ExampleDag $ mkUGraph [1..7] [(1,3),(1,2),(2,4),(3,5),(2,6),(5,2),(5,7)]

{-| Counter example obtained from running property tests and simplifying graph example found by QuickCheck -}
bftCounterExample:: ExampleDag () ()
bftCounterExample = ExampleDag $ mkUGraph [1,2,3] [(1,2),(1,3),(2,3)]


{-| ArbitraryDag is generated guaranteeing dagLongestPath.
  This type allows arbitrary vertex label a.

  ArbitraryDag generates a wide range of connected DAGs.
  The random construction selects longest path first and a core set of sorted vertices.
  Additional paths are generated at random but these paths use subsets of core vertices and, thus,
  are shorter.  Additional redundant edges are added randomly in a way that gurantees that 
  the longest path length is not going to increase.  
  These edges can introduce new vertices but they are made 'long' enough 
  (in the sense of topological order) to guarantee that the longest path length is not going to be impacted.  
-}
data ArbitraryDag a = ArbitraryDag {
   dagNodes :: [a]
   , dagEdges :: [(a, a)]
   , dagLongestPath :: [a]
} deriving Show

toExampleDag:: ArbitraryDag Int -> ExampleDag () ()
toExampleDag = ExampleDag . toGraph

toGraph :: ArbitraryDag Int -> UGr
toGraph tGr = mkUGraph (dagNodes tGr) (dagEdges tGr)

lineDag :: [a] -> ArbitraryDag a 
lineDag path = 
      let edgs = if null path 
                 then []
                 else take ((length path) - 1) path `zip` (tail path)
      in ArbitraryDag path edgs path


instance (Arbitrary a, Ord a, Random a) => Arbitrary (ArbitraryDag a) where
   arbitrary =
         let genArbitraryDag :: [a] -> Int -> Int -> Int -> ArbitraryDag a -> Gen (ArbitraryDag a)
             genArbitraryDag tnodes size cnt1 cnt2 dag =
                 if cnt1 > 0 
                 then 
                    do 
                      npath <- sublistOf tnodes
                      genArbitraryDag tnodes size (cnt1 - 1) cnt2 (addShorterPath npath dag)
                 else if cnt2 > 0 && size > 3
                      then
                        do
                          nodeInx <- choose (1, size - 1)
                          newNode <- choose (tnodes !! nodeInx, tnodes !! (size - 1))  
                          genArbitraryDag tnodes size cnt1 (cnt2 - 1) (addReduntandEdge (tnodes !! (nodeInx - 1), newNode) dag)
                      else 
                        return dag
             addShorterPath :: [a] -> ArbitraryDag a -> ArbitraryDag a
             addShorterPath path dag = 
                    let pathLength = length path
                        pathEdges = take (pathLength - 1) path `zip` (tail path)
                        l1 +++ l2 = nub (l1 ++ l2)
                    in if pathLength == 0 
                       then dag
                       else dag {dagEdges = (pathEdges +++ dagEdges dag)} 
             addReduntandEdge :: (a,a) -> ArbitraryDag a -> ArbitraryDag a
             addReduntandEdge (a1,a2) dag = 
                    dag  {dagNodes = nub (a2 : dagNodes dag),  dagEdges = (a1,a2) : dagEdges dag }            
         in do
              tnodes <- fmap (sort . nub) $ listOf arbitrary
              cnt1 <- arbitrary
              cnt2 <- arbitrary
              genArbitraryDag tnodes (length tnodes) cnt1 cnt2 (lineDag tnodes)         

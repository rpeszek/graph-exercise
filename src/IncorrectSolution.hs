{-
Copied from
https://stackoverflow.com/questions/40647260/finding-the-longest-path-through-a-dfs-forest-in-haskell
This solution correctly computes the longest path in a spanning tree but that is not the same as the longest path 
in the graph.  Graph can have many spanning trees of possibly different heights. 

Considering that longest path does not have good solution for general graphs
https://en.wikipedia.org/wiki/Longest_path_problem
just looking at type signature of the stackoverflow solution makes its validity a suspect. 

See test/IncorrectSolutionSpec.hs for counterexamples.
-}

module IncorrectSolution where

import Data.Graph.Inductive.Graph (Graph, Node)
import Data.Graph.Inductive.Query.BFS (bft)
import Data.Ord (comparing)
import Data.List (maximumBy, length)


bftLongestPath :: Graph gr => Node -> gr a b  -> [Node]
bftLongestPath node graph = maximumBy (comparing length) (bft node graph)

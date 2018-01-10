module Dag where

import Data.Graph.Inductive.Graph (Graph, Node)
import Data.Graph.Inductive.Query.DFS (topsort)

{- |
This class acts as a simple marker to provide some type safety that a graph is a DAG. 
Graph is a DAG iff it has topological sort.
The proof that returned list is sorted in topological order is left to developer (not type enforced).

Type safe DAGs are possible
https://www.reddit.com/r/haskell/comments/2u4w9c/typesafe_directed_acyclic_graphs/
but not pursued here.

fgl library implements a forgiving topsort in Data.Graph.Inductive.Query.DFS
If the graph has cycles, topsort does not guaranee that edges only move forward in the sorted list of nodes,
another words, topsort provides topologically sorted list of nodes if the graph is a DAG.
-}
class Graph g => Dag g where
   topologicalorder :: g a b -> [Node]
   topologicalorder = topsort

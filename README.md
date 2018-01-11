# graph-exercise

This project contains a solution to IPaC job interview exercise.
The exercise is to find longest path in a Directed Acyclic Graph.
 
This project provides  
* longestPath implemented as fold of topologically sorted Node list. 
(see [Solution.hs](/src/Solution.hs), [SolutionSpec.hs](/test/SolutionSpec.hs))
* Incorrect solution copied from [stackoverflow.hs](https://stackoverflow.com/questions/40647260/finding-the-longest-path-through-a-dfs-forest-in-haskell) that computes longest path in a spanning tree 
computed with BFS.  
(see [IncorrectSolution.hs](/src/IncorrectSolution.hs), [IncorrectSolutionSpec.hs](/test/IncorrectSolutionSpec.hs))
* Tests using ArbitraryDag (see [TestData.hs](/test/TestData.hs))
 
Third bullet is the interesting bit. `ArbitraryDag` provides a wide range of randomly generated 
graphs with known longest path.   
It was totally not obvious to me that the stackoverflow solution is incorrect.
I only found that after tests have failed.  
The stackoveflow code correctly computes longest path in 
a spanning tree, however, for some graphs, that is not the same as longest path in the graph.

Code
----
Both IncorrectSolution.hs and Solution.hs focus is declarative, concept driven code.  
`ArbitraryDag` is more imperative, willy nilly, brute force and could use more thinking.


Tests
-----
Here is a snapshot of example run
```
IncorrectSolution
  longest path using incorrect solution
    stackoverflow test match
    bftCounterExample fails FAILED [1]
    fails on arbitrary DAG FAILED [2]
Solution
  longest path using fold of topological sort
    bftCounterExample works
    works on arbitrary DAG

Failures:

  test/IncorrectSolutionSpec.hs:17: 
  1) IncorrectSolution, longest path using incorrect solution, bftCounterExample fails
       expected: [3,2,1]
        but got: [3,1]

  test/IncorrectSolutionSpec.hs:18: 
  2) IncorrectSolution, longest path using incorrect solution, fails on arbitrary DAG
       Falsifiable (after 4 tests): 
       ArbitraryDag {dagNodes = [-1,0,2], dagEdges = [(-1,2),(-1,0),(0,2)], dagLongestPath = [-1,0,2]}

Randomized with seed 1965771963

Finished in 0.4187 seconds
5 examples, 2 failures
```

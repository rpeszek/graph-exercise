{-#LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module Solution where

import Dag (Dag, topologicalorder)
import Data.Graph.Inductive.Graph (Node, suc)
import qualified Data.HashMap.Lazy as HM
import Data.Ord (comparing)
import Data.List (maximumBy, length, foldl')

type Path = [Node]

{-| 
This solution uses a straighforward fold of topologically sorted graph to compute longest path 
from specified node.  
-}
longestPath :: Dag gr => Node -> gr a b  -> Maybe Path
longestPath node graph = 
    let  children :: Node -> [Node] 
         children = suc graph
         logic ::  HM.HashMap Node Path -> Node -> HM.HashMap Node Path
         logic acc cnode =  
             let childrenPaths = map (flip (HM.lookupDefault []) acc) $ children cnode
                 path = if null childrenPaths 
                        then [cnode] 
                        else ((:) cnode) . maximumBy (comparing length) $ childrenPaths
             in HM.insert cnode path acc
         pathPerNode = foldl' logic HM.empty (reverse . topologicalorder $ graph)
    in HM.lookup node pathPerNode 
    
    

{-#LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module Solution where

import Dag (Dag, topologicalorder)
import Data.Graph.Inductive.Graph (Node, suc)
import qualified Data.HashMap.Lazy as HM
import Data.Ord (comparing)
import Data.List (length, foldl')
import Safe.Foldable (maximumByDef)

type Path = [Node]

{-| 
This solution uses a straighforward fold of topologically sorted graph to compute longest path 
from specified node.  
-}
longestPath :: Dag gr => Node -> gr a b  -> Maybe Path
longestPath node graph = 
    let  children :: Node -> [Node] 
         children = suc graph
         logic ::  Node -> HM.HashMap Node Path -> HM.HashMap Node Path
         logic cnode acc =  
             let path = ((:) cnode) . maximumByDef [] (comparing length) . map (flip (HM.lookupDefault []) acc) . children $ cnode
             in HM.insert cnode path acc
         pathPerNode = foldr logic HM.empty (topologicalorder graph)
    in HM.lookup node pathPerNode 
    
    

module Solve where

import Data.List
import Data.Char
-- import qualified Data.Graph as G
import qualified Data.Graph.Inductive as GI
import qualified Debug.Trace as DT

-- a node is simply an (Idx, Label) tuple
type NodeLabel = Char
type NodeIndex = Int
type NodeRecord = (NodeIndex, NodeLabel)
-- an edge is defined by the indicies of its start and end nodes and a label
type EdgeLabel = String
type EdgeRecord = (NodeIndex, NodeIndex, EdgeLabel)

type EdgeRecordHelper = (NodeLabel, NodeLabel)

-- parse the input, and return a list of edges
parseInput :: String -> [EdgeRecordHelper]
parseInput input = map makeEdge (lines input)
  where makeEdge line = (start line, end line)
        start line = head $ (words line) !! 1
        end line   = head $ (words line) !! 7 

-- conversion from node label to index
nodeLabelToNodeIndex :: NodeLabel -> NodeIndex
nodeLabelToNodeIndex lbl = (ord lbl) - (ord 'A') + 1

-- conversion from node index to node label
nodeIndexToNodeLabel :: NodeIndex -> NodeLabel
nodeIndexToNodeLabel idx = chr (idx + (ord 'A') - 1)

-- given a node label, construct the node 
nodeLabelToNode :: NodeLabel -> NodeRecord
nodeLabelToNode lbl = (nodeLabelToNodeIndex lbl, lbl)

nodeLabelsToNodes :: [NodeLabel] -> [NodeRecord]
nodeLabelsToNodes nodeLabels = map nodeLabelToNode nodeLabels 

edgesToNodeLabels :: [EdgeRecordHelper] -> [NodeLabel]
edgesToNodeLabels edges =  
  let 
    startNodeLabels = map (\(s,e) -> s) edges
    endNodeLabels   = map (\(s,e) -> e) edges
  in
    sort $ nub $ union startNodeLabels endNodeLabels


-- given a list of edges and an offset, extract the nodes, and build the DAG
-- buildGraph :: [Edge] -> Graph
buildGraph :: [EdgeRecordHelper] -> GI.Gr NodeLabel EdgeLabel -- Graph with node and edge label type
buildGraph edges = 
  let 
    nodeLabels = edgesToNodeLabels edges 
    nodes = nodeLabelsToNodes nodeLabels 
    --neighbourLabels nodeLabel = map (\n -> snd n) $ filter (\(start, end) -> start == nodeLabel) edges
    --neighbours nodeLabel = map snd $ map nodeLabelToNode $ neighbourLabels nodeLabel
    edgeList = map (\(start, end) -> (nodeLabelToNodeIndex start, nodeLabelToNodeIndex end, "")) edges
    {-edgeList = [
      (4, 8, ""),  
      (7, 4, ""),
      (5, 1, ""),
      (5, 7, ""),
      (1, 4, "")
     ] -}
  in
    GI.mkGraph nodes edgeList


-- check whether a given node has no incoming edges in the given graph
hasNoIncomingEdges :: GI.LNode NodeLabel -> GI.Gr NodeLabel EdgeLabel -> Bool
hasNoIncomingEdges (idx, _) graph = GI.indeg graph idx == 0

-- topological sort that - if more than one node is available - always uses the alphanumerically first one, i.e. if A and B are nodes with indegree = 0, take A first
myTopsort' :: GI.Gr NodeLabel EdgeLabel -> String
myTopsort' graph = 
  case candidates of
    [] -> ""
    (idx,lbl):xs -> [lbl] ++ (myTopsort' (GI.delNode idx graph) )
  where candidates = filter (\lnode -> hasNoIncomingEdges lnode graph) $ GI.labNodes graph

-- solve part I: parse input, build graph, find the path and return it
solveI :: String -> String
solveI input = sorted
  where graph = buildGraph (parseInput input) 
        sorted = myTopsort' graph

solveII = undefined

#if defined(STANDALONE)
main = do
    input <- readFile "input.txt"
    putStrLn $ show $ solveI  input 
    --putStrLn $ show $ solveII input 
#endif

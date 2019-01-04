module Solve where

import Data.List
import Data.Char
import qualified Data.Graph as G
import qualified Data.Graph.Inductive as GI
import qualified Debug.Trace as DT

type NodeLabel = String
type NodeRecord = (NodeLabel, Int)
-- an edge is defined by the labels of the nodes it connects
type EdgeRecord = (NodeLabel, NodeLabel)

-- parse the input, and return a list of edges
parseInput :: String -> [EdgeRecord]
parseInput input = map makeEdge (lines input)
  where makeEdge line = (start line, end line)
        start line = (words line) !! 1
        end line   = (words line) !! 7 

nodeLabelToNode :: NodeLabel -> NodeRecord
nodeLabelToNode lbl = (lbl, (ord $ head lbl) - (ord 'A') + 1)

nodeLabelsToNodes :: [NodeLabel] -> [NodeRecord]
nodeLabelsToNodes nodeLabels = map nodeLabelToNode nodeLabels 

edgesToNodeLabels :: [EdgeRecord] -> [NodeLabel]
edgesToNodeLabels edges =  
  let 
    startNodeLabels = map (\(s,e) -> s) edges
    endNodeLabels   = map (\(s,e) -> e) edges
  in
    sort $ nub $ union startNodeLabels endNodeLabels


-- given a list of edges and an offset, extract the nodes, and build the DAG
-- buildGraph :: [Edge] -> Graph
buildGraph :: [EdgeRecord] -> (G.Graph, G.Vertex -> (String, Int, [Int]), Int -> Maybe G.Vertex)
buildGraph edges = 
  let 
    nodeLabels = edgesToNodeLabels edges 
    nodes = nodeLabelsToNodes nodeLabels 
    neighbourLabels nodeLabel = map (\n -> snd n) $ filter (\(start, end) -> start == nodeLabel) edges
    neighbours nodeLabel = map snd $ map nodeLabelToNode $ neighbourLabels nodeLabel
    edgeList = map (\n -> (fst n, snd n, neighbours (fst n))) nodes
    {-edgeList = [
      ("node4",4,[8]),     -- the first component can be of any type
      ("node8",8,[]),
      ("node7",7,[4]),
      ("node5",5,[1,7]),
      ("node1",1,[4])
     ] -}
  in
    G.graphFromEdges edgeList


getFirst :: (a,b,c) -> a
getFirst (a, _, _) = a

-- solve part I: parse input, build graph, find the path and return it
solveI :: String -> String
solveI input = concat $ map (\n -> getFirst n) sorted
  where (graph, vertexToNode, keyToVertex) = buildGraph (parseInput input) 
        sorted = map vertexToNode $ G.topSort graph

solveII = undefined

{-
#if defined(STANDALONE)
main = do
    input <- readFile "input.txt"
    putStrLn $ show $ solveI  input 
    --putStrLn $ show $ solveII input 
#endif
-}

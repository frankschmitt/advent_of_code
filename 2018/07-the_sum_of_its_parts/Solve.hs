module Solve where

import Data.List

type Node = String
type Edge = (String,String)

{-
data Graph = MkGraph {
  nodes :: [Node],
  edges :: [Edge]
} 
-}
type Graph = ([Node], [Edge])

-- parse the input, and return a list of edges
parseInput :: String -> [Edge]
parseInput input = map makeEdge (lines input)
  where makeEdge line = (start line, end line)
        start line = (words line) !! 1
        end line   = (words line) !! 7 

-- given a list of edges, extract the nodes, and build the DAG
buildGraph :: [Edge] -> Graph
buildGraph edges = 
  let 
    startNodes = map (\(s,e) -> s) edges
    endNodes   = map (\(s,e) -> e) edges
    nodes = nub $ union startNodes endNodes 
  in
    (nodes,edges)

-- find the path in the given graph
findPath :: Graph -> [Node]
findPath (nodes, edges) = 
  case (nodes,edges) of 
    ([],_) -> []      -- no nodes left -> return empty list
    (nodes', []) -> sort nodes' -- no edges left -> sort the remaining nodes, and return them
    (nodes', edges') -> [firstNode] ++ findPath (reducedNodes, reducedEdges)
      where firstNode = head $ sort $ candidateNodes
            reducedNodes = nodes' \\ [firstNode]
            reducedEdges = filter (\(n1,n2) -> n1 /= firstNode && n2 /= firstNode) edges'
            candidateNodes = filter hasNoIncomingEdges nodes'
            hasNoIncomingEdges n = all (\(_,end) ->  n /= end) edges'


solveI :: String -> String
solveI input = concat $ findPath graph
  where graph = buildGraph $ (parseInput input)

solveII = undefined


#if defined(STANDALONE)
main = do
    input <- readFile "input.txt"
    putStrLn $ show $ solveI  input 
    --putStrLn $ show $ solveII input 
#endif


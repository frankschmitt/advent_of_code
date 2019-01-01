module Solve where

import Data.List
import Data.Char
import qualified Debug.Trace as DT

type NodeLabel = String
-- a node is defined by its label and its delay
type Node = (NodeLabel, Int)
-- an edge is defined by the labels of the nodes it connects
type Edge = (NodeLabel, NodeLabel)

type Second = Int
type WorkerID = Int

-- a worker is either idle, or has been working on a task for N seconds, or has finished working
data WorkerState = Idle | Working Node Second | Done Node
  deriving (Show, Eq)
-- a worker is defined by their id and their state
type Worker = (WorkerID, WorkerState) 

-- overall system state: we need to keep track of the graph, the workers and the current time
type SystemState = (Graph, [Worker], Second)

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

-- given a list of edges and an offset, extract the nodes, and build the DAG
buildGraph :: [Edge] -> Int -> Graph
buildGraph edges offset = 
  let 
    startNodeLabels = map (\(s,e) -> s) edges
    endNodeLabels   = map (\(s,e) -> e) edges
    nodeLabels = nub $ union startNodeLabels endNodeLabels
    nodes = map (\lbl -> (lbl, offset + (ord $ head lbl) - (ord 'A') + 1)) nodeLabels
  in
    (nodes,edges)

-- remove a node from a graph, and return the reduced graph
removeNode :: Graph -> Node -> Graph
removeNode (nodes, edges) n = (reducedNodes, reducedEdges)
  where reducedNodes = nodes \\ [n]
        reducedEdges = filter (\(n1,n2) -> n1 /= (fst n) && n2 /= (fst n)) edges

-- find candidate nodes (those without incoming edges)
findCandidateNodes :: Graph -> [Node]
findCandidateNodes (nodes, edges) = filter hasNoIncomingEdges nodes
  where hasNoIncomingEdges n = all (\(_,end) ->  (fst n) /= end) edges

-- next node to process 
nextCandidateNode :: Graph -> Node
nextCandidateNode (nodes', edges') = head $ sort $ findCandidateNodes (nodes', edges')

-- find the path in the given graph
findPath :: Graph -> [Node]
findPath (nodes, edges) = 
  case (nodes,edges) of 
    ([],_) -> []      -- no nodes left -> return empty list
    (nodes', []) -> sort nodes' -- no edges left -> sort the remaining nodes, and return them
    (nodes', edges') -> [firstNode] ++ findPath (removeNode (nodes', edges') firstNode)
      where firstNode = nextCandidateNode (nodes', edges') 

-- solve part I: parse input, build graph, find the path and return it
solveI :: String -> String
solveI input = concat $ map (\(lbl, delay) -> lbl) $ findPath graph
  where graph = buildGraph (parseInput input) 0

-- given a list of workers, get the maximum remaining work
getMaxRemainingWork :: [Worker] -> Int
getMaxRemainingWork workers = maximum $ map remainingWork workers
  where remainingWork (_, state) = case state of
         Idle -> 0
         Done _ -> 0
         Working task since -> (snd task) - since  

availableWorkers :: [Worker] -> [Worker]
availableWorkers workers= filter isAvailable workers
  where isAvailable (_, state) = case state of 
                                    Working _ _ -> False 
                                    _ -> True

assignNextTask :: SystemState -> SystemState
assignNextTask (graph, workers, second) = 
  let
    nextTask = nextCandidateNode graph
    nextWorker = head $ availableWorkers workers
    (nwID, _)  = nextWorker
    newWorker = (nwID, Working nextTask 0)
    updatedWorkers = map (\(id, state) -> 
                       if id /= nwID then (id, state)
                       else newWorker 
                     ) workers       
  in
    DT.trace ("assignNextTask: task = " ++ (show nextTask) ++ ", worker: " ++ (show newWorker)) (removeNode graph nextTask, updatedWorkers, second)    

nextStepWorkers :: [Worker] -> [Worker]
nextStepWorkers workers = DT.trace ("nextStepWorkers, before: " ++ (show workers) ++ ", after: " ++ (show newWorkers)) newWorkers
  where nextStep (id, state) = case state of
                                 Idle -> (id, state)
                                 Done _ -> (id, state)
                                 Working (lbl, delay) since -> if delay == since then (id, Done (lbl, delay))
                                                               else (id, Working (lbl, delay) (since + 1))
        newWorkers = (map nextStep workers)
         

-- run simulation for part II step-by-step; terminate if work has been completed
solveII' :: SystemState -> Int
solveII' (graph, workers, second) = 
  DT.trace ("solveII', second: " ++ (show second))(case (graph, availableWorkers workers) of
                                                     (([], _), _) -> second + getMaxRemainingWork workers -- all nodes finished or being processed  
                                                     ((nodes', edges'), []) -> solveII' (graph, nextStepWorkers workers, second + 1)
                                                     ((nodes', edges'), w)  -> solveII' (assignNextTask (graph, workers, second)))

-- solve part II: parse input, build graph with offsets, run simulation step-by-step
solveII :: String -> Int -> Int -> Int
solveII input numWorkers offset = solveII' (graph, workers, 0) 
  where graph = buildGraph (parseInput input) offset
        workers = take numWorkers $ zip [1..] (repeat Idle)


#if defined(STANDALONE)
main = do
    input <- readFile "input.txt"
    putStrLn $ show $ solveI  input 
    --putStrLn $ show $ solveII input 
#endif


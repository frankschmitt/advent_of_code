-- required for compile-time check of regexes
{-# LANGUAGE QuasiQuotes                      #-}

module Solve where


import qualified Data.IntMap as IM
import qualified GHC.Arr as A
import Data.List
import Text.RE.Replace
import Text.RE.TDFA.String
import qualified Debug.Trace as DT

-- TYPES
type Timestamp = Int
type GuardIndex = Int

data Action = BeginsShift GuardIndex | FallsAsleep | WakeUp
  deriving (Show,Eq)


data Event = MkEvent {
  timestamp :: Timestamp,
  action :: Action
} deriving (Show, Eq)

-- STATES during parsing
data GuardState = Init | Awake GuardIndex | AsleepSince Timestamp GuardIndex

data SystemState = MkSystemState {
  guardState :: GuardState,
  -- guards :: [Guard]
  guards :: IM.IntMap [Timestamp]
}

initState :: SystemState
initState = MkSystemState Init IM.empty

-- given an old list of guards, a guard, start and end timestamp, update the sleep times for this guard, and return the updated list
updateSleepTimes :: IM.IntMap [Timestamp] -> GuardIndex -> Timestamp -> Timestamp -> IM.IntMap [Timestamp]
updateSleepTimes oldGuards guardIndex startTimestamp endTimestamp = IM.insertWithKey (\key new_val old_val -> old_val ++ new_val) guardIndex [startTimestamp .. (endTimestamp - 1)] oldGuards

-- state transition
nextState :: SystemState -> Event -> SystemState
nextState oldState event = case (guardState oldState, action event) of
    (Init, BeginsShift g)  -> MkSystemState (Awake g) IM.empty
    (Awake g, FallsAsleep) -> MkSystemState (AsleepSince (timestamp event) g) (guards oldState)
    (AsleepSince ts g, WakeUp) -> MkSystemState (Awake g) (updateSleepTimes (guards oldState) g ts (timestamp event)) 
    (Awake _, BeginsShift g) -> MkSystemState (Awake g) (guards oldState)
    _ -> initState

-- parsing Input (this time, with regexes)
-- Example input: 
-- [1518-09-29 00:35] falls asleep
-- [1518-10-14 00:00] Guard #2927 begins shift
-- [1518-07-20 00:53] wakes up
-- [1518-04-02 00:10] falls asleep
parseLine :: String -> Event
{-parseLine line = MkEvent 0 FallAsleep 
  where regex = [re|.*${minute}([0-9]{2}) ${action}(falls asleep|wakes up|Guard #${index}([0-9]+) begins shift)|]
        (_, _, _, groups)  = line =~ regex :: (String, String, String, [String]) -- before, match, after, [groups]
        minute = groups !! 0
        action = groups !! 1
-}
parseLine line =
  case action of
    "falls asleep" -> MkEvent minute FallsAsleep
    "wakes up"     -> MkEvent minute WakeUp
    _              -> MkEvent minute (BeginsShift guardIdx) 
  where regex = [re|\[.*${minute}([0-9]{2})\] ${action}(falls asleep|wakes up|Guard #${index}([0-9]+) begins shift)|]
        (_, _, _, groups)  = line =~ regex :: (String, String, String, [String]) -- before, match, after, [groups]
        g = DT.trace (" parsing " ++ (show line) ++ ", match: " ++ (show groups))
        minute = read (groups !! 0) :: Timestamp
        action = groups !! 1
        guardIdx = read (groups !! 2) :: Int
       
prepSolution :: String -> SystemState
prepSolution input = prep' initState (sort (lines input))
 where prep' currentState [] = currentState
       prep' currentState (x:xs) = prep' (nextState currentState (parsed x)) xs
       parsed x = DT.trace ("parsing " ++ (show x)) (parseLine x)

findSleepiestGuard :: IM.IntMap [Timestamp]-> (Int, [Timestamp])  
findSleepiestGuard sleepTimes = head $ reverse $ sortBy (\(_,times1) (_,times2) -> compare (length times1) (length times2)) $ IM.assocs sleepTimes

findMostFrequent :: [Timestamp] -> Timestamp
findMostFrequent ts = head $ map fst $ sortedByCount 
  where sorted = sort ts
        grouped = groupBy (==) sorted
        keyWithCount = map (\lst -> (head lst, length lst)) grouped
        sortedByCount = sortBy (\(_,cnt1) (_,cnt2) -> compare cnt2 cnt1) $ keyWithCount
  
solveI :: String -> Int
solveI input = guardIdx * minute 
  where solution = prepSolution input
        (guardIdx, sleepTimes) = findSleepiestGuard (guards solution)
        minute = findMostFrequent sleepTimes

solveII = undefined

#if defined(STANDALONE)
main = do
    input <- readFile "input.txt"
    putStrLn $ show $ solveI input
    -- putStrLn $ show $ (solveII input )
#endif

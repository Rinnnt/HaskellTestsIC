module Alloc where

import Data.Maybe
import Data.List

import Types
import Examples

------------------------------------------------------
--
-- Part I
--
count :: Eq a => a -> [a] -> Int
count
  = (length .) . elemIndices

degrees :: Eq a => Graph a -> [(a, Int)]
degrees (ns, es)
  = map (\n -> (n, count n flat)) ns
  where
    flat = concatMap (\(x, y) -> [x, y]) es

neighbours :: Eq a => a -> Graph a -> [a]
neighbours n (ns, es)
  = [if (n == n') then n'' else n' | (n', n'') <- es, n == n' || n == n'']

removeNode :: Eq a => a -> Graph a -> Graph a
removeNode n (ns, es)
  = (delete n ns, [e | e@(n', n'') <- es, n /= n' && n /= n''])

------------------------------------------------------
--
-- Part II
--
colourGraph :: (Ord a, Show a) => Int -> Graph a -> Colouring a
colourGraph c ([], [])
  = []
colourGraph c g@(ns, es)
  = (n, givenc) : colours
  where
    degs    = degrees g
    (n, d)  = foldl1 (\k@(_, d) k'@(_, d') -> if (d' < d) then k' else k) degs
    nbs     = neighbours n g
    colours = colourGraph c (removeNode n g)
    takencs = [c | (n, c) <- colours, n `elem` nbs]
    givenc  = head (([1..c] \\ takencs) ++ [0])


------------------------------------------------------
--
-- Part III
--
buildIdMap :: Colouring Id -> IdMap
buildIdMap 
  = undefined

buildArgAssignments :: [Id] -> IdMap -> [Statement]
buildArgAssignments 
  = undefined

renameExp :: Exp -> IdMap -> Exp
-- Pre: A precondition is that every variable referenced in 
-- the expression is in the idMap. 
renameExp 
  = undefined

renameBlock :: Block -> IdMap -> Block
-- Pre: A precondition is that every variable referenced in 
-- the block is in the idMap. 
renameBlock 
  = undefined

renameFun :: Function -> IdMap -> Function
renameFun (f, as, b) idMap
  = (f, as, buildArgAssignments as idMap ++ renameBlock b idMap)

-----------------------------------------------------
--
-- Part IV
--
buildIG :: [[Id]] -> IG
buildIG 
  = undefined

-----------------------------------------------------
--
-- Part V
--
liveVars :: CFG -> [[Id]]
liveVars 
  = undefined

buildCFG :: Function -> CFG
buildCFG 
  = undefined

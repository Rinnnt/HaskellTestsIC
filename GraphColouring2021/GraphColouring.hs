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
  = (n, givenc) : colouring
  where
    degs      = degrees g
    (n, d)    = foldl1 (\k@(_, d) k'@(_, d') -> if (d' < d) then k' else k) degs
    nbs       = neighbours n g
    colouring = colourGraph c (removeNode n g)
    takencs   = [c | (n, c) <- colouring, n `elem` nbs]
    givenc    = head (([1..c] \\ takencs) ++ [0])


------------------------------------------------------
--
-- Part III
--
buildIdMap :: Colouring Id -> IdMap
buildIdMap colouring
  = ("return", "return") : map buildId colouring
  where
    buildId :: (Id, Int) -> (Id, Id)
    buildId (id, 0) = (id, id)
    buildId (id, n) = (id, 'R' : (show n))

buildArgAssignments :: [Id] -> IdMap -> [Statement]
buildArgAssignments args idmap
  = map buildAssign args
  where
    buildAssign :: Id -> Statement
    buildAssign id = Assign (lookUp id idmap) (Var id)

renameExp :: Exp -> IdMap -> Exp
-- Pre: A precondition is that every variable referenced in 
-- the expression is in the idMap. 
renameExp c@Const{} _ =
  c
renameExp (Var id) idmap =
  Var (lookUp id idmap)
renameExp (Apply op e e') idmap =
  Apply op (renameExp e idmap) (renameExp e' idmap)

renameBlock :: Block -> IdMap -> Block
-- Pre: A precondition is that every variable referenced in 
-- the block is in the idMap. 
renameBlock [] _ = []
renameBlock ((Assign id e) : ss) idmap =
  if (Var var == val) then rem else (Assign var val) : rem
  where
    var = lookUp id idmap
    val = renameExp e idmap
    rem = renameBlock ss idmap
renameBlock ((If e b b') : ss) idmap =
  If (renameExp e idmap) (renameBlock b idmap) (renameBlock b' idmap) : (renameBlock ss idmap)
renameBlock ((While e b) : ss) idmap =
  While (renameExp e idmap) (renameBlock b idmap) : (renameBlock ss idmap)

renameFun :: Function -> IdMap -> Function
renameFun (f, as, b) idMap
  = (f, as, buildArgAssignments as idMap ++ renameBlock b idMap)

-----------------------------------------------------
--
-- Part IV
--
buildIG :: [[Id]] -> IG
buildIG vars =
  (ns, es)
  where
    ns = (nub . concat) vars
    es = concatMap edgesFrom ns

    edgesFrom :: Id -> [Edge Id]
    edgesFrom id =
      map (id,) (nub (concatMap (toRightOf id) vars))

    toRightOf :: Eq a => a -> [a] -> [a]
    toRightOf x [] =
      []
    toRightOf x (y : ys) =
      if (x == y) then ys else toRightOf x ys

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

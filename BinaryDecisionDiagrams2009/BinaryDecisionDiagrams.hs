import Data.List

type Index = Int

data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)

type Env = [(Index, Bool)]

type NodeId = Int

type BDDNode =  (NodeId, (Index, NodeId, NodeId))

type BDD = (NodeId, [BDDNode])

------------------------------------------------------
-- PART I

-- Pre: The item is in the given table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp k
  = (snd . head) . (filter ((== k) . fst))

checkSat :: BDD -> Env -> Bool
checkSat (rootId, nodes) env
  = go rootId == 1
  where
    go :: NodeId -> NodeId
    go 0 = 0
    go 1 = 1
    go nodeId
      | lookUp index env = go tId
      | otherwise        = go fId
      where
        (index, fId, tId) = lookUp nodeId nodes
        

sat :: BDD -> [[(Index, Bool)]]
sat (rootId, nodes)
  = all rootId
  where
    all :: NodeId -> [Env]
    all 0 = []
    all 1 = [[]]
    all nodeId
      = map ((index, False) :) (all fId) ++ map ((index, True) :) (all tId)
      where
        (index, fId, tId) = lookUp nodeId nodes
    

------------------------------------------------------
-- PART II

simplify :: BExp -> BExp
simplify (Not (Prim x))          = Prim (not x)
simplify (Or (Prim x) (Prim y))  = Prim (x || y)
simplify (And (Prim x) (Prim y)) = Prim (x && y)
simplify x                       = x

restrict :: BExp -> Index -> Bool -> BExp
restrict b@(Prim x)  k v = b
restrict b@(IdRef x) k v = if k == x then Prim v else b
restrict (Not b)     k v = simplify (Not (restrict b k v))
restrict (And b b')  k v = simplify (And (restrict b k v) (restrict b' k v))
restrict (Or b b')   k v = simplify (Or (restrict b k v) (restrict b' k v))

------------------------------------------------------
-- PART III

-- Pre: Each variable index in the BExp appears exactly once
--     in the Index list; there are no other elements
-- The question suggests the following definition (in terms of buildBDD')
-- but you are free to implement the function differently if you wish.
buildBDD :: BExp -> [Index] -> BDD
buildBDD e xs
  = buildBDD' e 2 xs

-- Potential helper function for buildBDD which you are free
-- to define/modify/ignore/delete/embed as you see fit.
buildBDD' :: BExp -> NodeId -> [Index] -> BDD
buildBDD' (Prim False) _ [] = (0, [])
buildBDD' (Prim True)  _ [] = (1, [])
buildBDD' e nodeId (x : xs)
  = (nodeId, (nodeId, (x, lId, rId)) : lnodes ++ rnodes)
  where
    (lId, lnodes) = buildBDD' (restrict e x False) (2 * nodeId) xs
    (rId, rnodes) = buildBDD' (restrict e x True) (2 * nodeId + 1) xs

------------------------------------------------------
-- PART IV

-- Pre: Each variable index in the BExp appears exactly once
--      in the Index list; there are no other elements
buildROBDD :: BExp -> [Index] -> BDD
buildROBDD e xs
  = optimise (buildROBDD' e 2 xs)

-- Bugged, does not apply change everywhere
optimise :: BDD -> BDD
optimise (rootId, allNodes)
  | reverse allNodes == optimised = (rootId, reverse optimised)
  | otherwise                     = optimise (rootId, reverse optimised)
  where
    optimised = optimise' (reverse allNodes)

    -- Try getting nodes to change
    -- then change everywhere
    toChange :: [BDDNode] -> [BDDNode]
    toChange [] = []
    toChange (n@(nodeId, refs@(x, l, r)) : nodes)
      | l == r = [n]
      | not (null sameNode) = [n]
      otherwise = toChange nodes
      where
        sameNode = filter ((== refs) . snd) nodes
    
    optimise' :: [BDDNode] -> [BDDNode]
    optimise' [] = []
    optimise' (n@(nodeId, refs@(x, l, r)): nodes)
      | l == r              = changed'
      | not (null sameNode) = changed
      | otherwise           = n : optimise' nodes
      where
        sameNode = filter ((== refs) . snd) nodes
        changed  = [ change node | node <- nodes]
        changed'  = [ change' node | node <- nodes]

        change ::  BDDNode -> BDDNode
        change u@(nId, (xId, lId, rId))
          | lId == nodeId = (nId, (xId, (fst (head sameNode)), rId))
          | rId == nodeId = (nId, (xId, lId, (fst (head sameNode))))
          | otherwise     = u

        change' :: BDDNode -> BDDNode
        change' u@(nId, (xId, lId, rId))
          | lId == nodeId = (nId, (xId, l, rId))
          | rId == nodeId = (nId, (xId, lId, r))
          | otherwise     = u

buildROBDD' :: BExp -> NodeId -> [Index] -> BDD
buildROBDD' (Prim False) _ [] = (0, [])
buildROBDD' (Prim True)  _ [] = (1, [])
buildROBDD' e nodeId (x : xs)
  | lId == rId = (lId, lnodes)
  | otherwise  = (nodeId, (nodeId, (x, lId, rId)) : lnodes ++ rnodes)
  where
    (lId, lnodes) = buildROBDD' (restrict e x False) (2 * nodeId) xs
    (rId, rnodes) = buildROBDD' (restrict e x True) (2 * nodeId + 1) xs
    

------------------------------------------------------
-- Examples for testing...

b1, b2, b3, b4, b5, b6, b7, b8 :: BExp
b1 = Prim False
b2 = Not (And (IdRef 1) (Or (Prim False) (IdRef 2)))
b3 = And (IdRef 1) (Prim True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
bdd3 = (5,[(5,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])


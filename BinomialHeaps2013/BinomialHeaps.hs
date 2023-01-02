type BinHeap a = [BinTree a]

data BinTree a = Node a Int (BinHeap a)
               deriving (Eq, Ord, Show)

--------------------------------------------------------------
-- PART I

key :: BinTree a -> a
key (Node k _ _)
  = k

rank :: BinTree a -> Int
rank (Node _ r _)
  = r

children :: BinTree a -> [BinTree a]
children (Node _ _ h)
  = h

combineTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
combineTrees t@(Node k r h) t'@(Node k' r' h')
  | r /= r'   = error "Requires binomial trees of the same rank"
  | k < k'    = Node k (r + 1) (t' : h)
  | otherwise = Node k' (r + 1) (t : h')

--------------------------------------------------------------
-- PART II

extractMin :: Ord a => BinHeap a -> a
extractMin h
  = foldr1 min (map key h)

mergeHeaps :: Ord a => BinHeap a -> BinHeap a -> BinHeap a
mergeHeaps [] h' = h'
mergeHeaps h []  = h
mergeHeaps h@(t : ts) h'@(t' : ts')
  | r < r'    = t : mergeHeaps ts h'
  | r' < r    = t' : mergeHeaps h ts'
  | otherwise = mergeHeaps c (mergeHeaps ts ts')
  where
    r  = rank t
    r' = rank t'
    c  = [combineTrees t t']

insert :: Ord a => a -> BinHeap a -> BinHeap a
insert x h
  = mergeHeaps [Node x 0 []] h

deleteMin :: Ord a => BinHeap a -> BinHeap a
deleteMin h
  = mergeHeaps cs ts
  where
    (cs, ts) = rem h
    
    rem :: Ord a => BinHeap a -> (BinHeap a, BinHeap a)
    rem h@(t : ts)
      | key t == mk = (reverse (children t), ts)
      | otherwise   = (cs, t : ts')
      where
        mk = extractMin h
        (cs, ts') = rem ts

{-
remove :: Eq a => a -> BinHeap a -> BinHeap a
remove k []
  = []
remove k (t : ts)
  | key t == k = ts
  | otherwise  = t : remove k ts

removeMin :: Ord a => BinHeap a -> (BinTree a, BinHeap a)
removeMin h
  = (head (filter ((== mk) . key) h), remove mk h)
  where
    mk = extractMin h
-}

binSort :: Ord a => [a] -> [a]
binSort xs
  = extractSorted h
  where
    h = foldr insert [] xs
    
    extractSorted :: Ord a => BinHeap a -> [a]
    extractSorted [] = []
    extractSorted h  = extractMin h : extractSorted (deleteMin h)

--------------------------------------------------------------
-- PART III

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

toBinary :: BinHeap a -> [Int]
toBinary h
  = reverse (map (boolToInt . hasRank) [0..maxRank])
  where
    ranks   = map rank h
    maxRank = foldr1 max ranks
    hasRank = (flip elem) ranks

pad :: [Int] -> [Int] -> ([Int], [Int])
pad bs bs'
  | l == l' = (bs, bs')
  | l < l'  = pad (0 : bs) bs'
  | l > l'  = pad bs (0 : bs')
  where
    l  = length bs
    l' = length bs'

binarySum :: [Int] -> [Int] -> [Int]
binarySum bs bs'
  | c == 1    = 1 : sum
  | otherwise = sum
  where
    (pbs, pbs') = pad bs bs'
    (sum, c)    = foldr fullAdder ([], 0) (zipWith (+) pbs pbs')

    fullAdder :: Int -> ([Int], Int) -> ([Int], Int)
    fullAdder b (bs, c)
      = (r : bs, q)
      where
        (q, r) = (b + c) `divMod` 2

------------------------------------------------------
-- Some sample trees...

t1, t2, t3, t4, t5, t6, t7, t8 :: BinTree Int
-- Note: t7 is the result of merging t5 and t6

-- t1 to t4 appear in Figure 1...
t1 = Node 4 0 []
t2 = Node 1 1 [Node 5 0 []]
t3 = Node 2 2 [Node 8 1 [Node 9 0 []], 
               Node 7 0 []]
t4 = Node 2 3 [Node 3 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- t5 and t6 are on the left of Figure 2; t7 is on the
-- right
t5 = Node 4 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []]
t6 = Node 2 2 [Node 8 1 [Node 9 0 []], Node 7 0 []]
t7 = Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []], Node 10 0 []],
               Node 8 1 [Node 9 0 []], 
               Node 7 0 []]

-- An additional tree...
t8 = Node 12 1 [Node 16 0 []]

------------------------------------------------------
-- Some sample heaps...

h1, h2, h3, h4, h5, h6, h7 :: BinHeap Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 2 [Node 12 1 [Node 16 0 []],
                Node 5 0 []],
      Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 8 1 [Node 9 0 []],
                Node 7 0 []]]

-- h3 is shown in Figure 3...
h3 = [t1, t2, t4]

-- Two additional heaps, used below. They are shown
-- in Figure 4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 is the result of merging h4 and h5, shown in Figure 4(b)...
h6 = [Node 4 0 [],
      Node 1 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 12 1 [Node 16 0 []],
                Node 5 0 []]]

-- h7 is shown in Figure 5...
h7 = [Node 4 3 [Node 4 2 [Node 12 1 [Node 16 0 []],
                          Node 5 0 []],
                Node 6 1 [Node 8 0 []],
                Node 10 0 []]]




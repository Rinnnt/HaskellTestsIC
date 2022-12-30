data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Show)

------------------------------------------------------

isPrefix :: String -> String -> Bool
isPrefix x y
  = x == take (length x) y

removePrefix :: String -> String -> String
removePrefix x y
--Pre: s is a prefix of s'
  = drop (length x) y

suffixes :: [a] -> [[a]]
suffixes s
  = take (length s) (iterate tail s)

isSubstring :: String -> String -> Bool
isSubstring x y
  = any (isPrefix x) (suffixes y)

findSubstrings :: String -> String -> [Int]
findSubstrings x y
  = [i | (s, i) <- zip (suffixes y) [0..], isPrefix x s]

findSubstrings2 :: String -> String -> [Int]
findSubstrings2 x y
  = foldl addIndex [] (zip (suffixes y) [0..])
  where
    addIndex :: [Int] -> (String, Int) -> [Int]
    addIndex a (s, i)
      | isPrefix x s = a ++ [i]
      | otherwise    = a

------------------------------------------------------

getIndices :: SuffixTree -> [Int]
getIndices (Leaf x)
  = [x]
getIndices (Node ts)
  = concatMap getIndices (map snd ts)

partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition [] y = ([], [], y)
partition x [] = ([], x, [])
partition x@(c : s) y@(c' : s')
  | c == c'   = (c : pref, rem, rem')
  | otherwise = ([], x, y)
  where
    (pref, rem, rem') = partition s s'

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' "" (Leaf x)
  = [x]
findSubstrings' "" (Node ts)
  = concatMap (findSubstrings' "") (map snd ts)
findSubstrings' s (Leaf x)
  = []
findSubstrings' s (Node [])
  = []
findSubstrings' s (Node ((s', t) : ts))
  | null r || null r'   = findSubstrings' r t ++ findSubstrings' s (Node ts)
  | otherwise           = findSubstrings' s (Node ts)
  where
    (p, r, r') = partition s s'

------------------------------------------------------

insert :: (String, Int) -> SuffixTree -> SuffixTree
insert s (Node ts)
  = Node (insert' s ts)
  where
    insert' :: (String, Int) -> [(String, SuffixTree)] -> [(String, SuffixTree)]
    insert' (s, i) []
      = [(s, Leaf i)]
    insert' (s, i) ((s', t) : ts)
      | null p    = (s', t) : insert' (s, i) ts
      | null r'   = (s', insert (r, i) t) : ts
      | otherwise = (p, Node [(r, Leaf i), (r', t)]) : ts
      where
        (p, r, r') = partition s  s'

-- This function is given
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

------------------------------------------------------
-- Part IV

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring 
  = undefined

------------------------------------------------------
-- Example strings and suffix trees...

s1 :: String
s1 
  = "banana"

s2 :: String
s2 
  = "mississippi"

t1 :: SuffixTree
t1 
  = Node [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 
  = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]

    

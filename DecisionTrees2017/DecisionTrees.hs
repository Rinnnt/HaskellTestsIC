import Data.Maybe
import Data.List

type AttName = String

type AttValue = String

type Attribute = (AttName, [AttValue])

type Header = [Attribute]

type Row = [AttValue]

type DataSet = (Header, [Row])

data DecisionTree = Null |
                    Leaf AttValue | 
                    Node AttName [(AttValue, DecisionTree)]
                  deriving (Eq, Show)

type Partition = [(AttValue, DataSet)]

type AttSelector = DataSet -> Attribute -> Attribute

xlogx :: Double -> Double
xlogx p
  | p <= 1e-100 = 0.0
  | otherwise   = p * log2 p 
  where
    log2 x = log x / log 2

lookUp :: (Eq a, Show a, Show b) => a -> [(a, b)] -> b
lookUp x table
  = fromMaybe (error ("lookUp error - no binding for " ++ show x ++ 
                      " in table: " ++ show table))
              (lookup x table)

--------------------------------------------------------------------
-- PART I
--------------------------------------------------------------------

allSame :: Eq a => [a] -> Bool
allSame xs = length (nub xs) < 2
{-
allSame [] = True
allSame xs
  = all (==head xs) xs
-}

remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove k' kvs
  = [(k, v) | (k, v) <- kvs, k /= k']

lookUpAtt :: AttName -> Header -> Row -> AttValue
--Pre: The attribute name is present in the given header.
lookUpAtt att h r
  = lookUp att zipped
  where
    zipped = zip [fst v | v <- h]  r
{-
lookUpAtt name header row
  = head (intersect atts row)
  where
    atts = lookUp name header
-}

removeAtt :: AttName -> Header -> Row -> Row
removeAtt att h r
  = [b | (a, b) <- remove att zipped]
  where
    zipped = zip [fst v | v <- h] r

addToMapping :: Eq a => (a, b) -> [(a, [b])] -> [(a, [b])]
addToMapping (k, v) [] = [(k, [v])]
addToMapping (k, v) ((k', v') : kvss)
  | k == k'   = ((k, v : v') : kvss)
  | otherwise = (k', v') : addToMapping (k, v) kvss

buildFrequencyTable :: Attribute -> DataSet -> [(AttValue, Int)]
--Pre: Each row of the data set contains an instance of the attribute
buildFrequencyTable (attName, attVals) ds
  = map (\(a, b) -> (a, length b)) (count ds)
  where
    count :: DataSet -> [(AttValue, String)]
    count (h, []) = [(val, "") | val <- attVals]
    count (h, (r : rs)) = addToMapping (value, 'a') (count (h, rs))
      where
        value = lookUpAtt attName h r

buildFrequencyTable' :: Attribute -> DataSet -> [(AttValue, Int)]
buildFrequencyTable' (attName, attVals) (h, rs)
  = [(head v, length v - 1) | v <- group (sort (attVals ++ [val | val <- attVals, r <- rs, val == lookUpAtt attName h r]))]
   

--    cs = transpose (map (zip [fst v | v <- h]) rs)

--------------------------------------------------------------------
-- PART II
--------------------------------------------------------------------

nodes :: DecisionTree -> Int
nodes Null         = 0
nodes (Leaf x)     = 1
nodes (Node _ vts) = 1 + sum [nodes t | (v, t) <- vts]

evalTree :: DecisionTree -> Header -> Row -> AttValue
evalTree Null _ _               = ""
evalTree (Leaf x) _ _           = x
evalTree (Node attName vts) h r = evalTree (lookUp val vts) h r
  where
    val = lookUpAtt attName h r

--------------------------------------------------------------------
-- PART III
--------------------------------------------------------------------

--
-- Given...
-- In this simple case, the attribute selected is the first input attribute 
-- in the header. Note that the classifier attribute may appear in any column,
-- so we must exclude it as a candidate.
--
nextAtt :: AttSelector
--Pre: The header contains at least one input attribute
nextAtt (header, _) (classifierName, _)
  = head (filter ((/= classifierName) . fst) header)

partitionData :: DataSet -> Attribute -> Partition
partitionData ds@(h, rs) (attName, attVals)
  = map (\(aV, rs) -> (aV, (newHeader, rs))) (partitions ds [(val, []) | val <- attVals])
  where
    partitions :: DataSet -> [(AttValue, [Row])] -> [(AttValue, [Row])]
    partitions (h, []) p = p
    partitions (h, (r : rs)) p = addToMapping (lookUpAtt attName h r, newr) (partitions (h, rs) p)
      where
        newr = removeAtt attName h r
    newHeader = remove attName h
    

buildTree :: DataSet -> Attribute -> AttSelector -> DecisionTree 
buildTree (h, [])  _ _ = Null
buildTree ds@(h, rs) att@(attName, _) attSel
  | length classifierVals == 1 = Leaf (head classifierVals)
  | otherwise =  Node nextAttName [(nextAttVal, buildTree ds' att attSel) | (nextAttVal, ds') <- partitionData ds nextAtt]
  where
    classifierVals = nub [lookUpAtt attName h r | r <- rs]
    nextAtt@(nextAttName, _) = attSel ds att
    

--------------------------------------------------------------------
-- PART IV
--------------------------------------------------------------------

entropy :: DataSet -> Attribute -> Double
entropy (h, []) _ = 0.0
entropy ds@(h, rs) att@(attName, attVals)
  = sum [negate (xlogx ((p v))) | v <- attVals]
  where
    p :: AttValue -> Double
    p v = fromIntegral (lookUp v ft) / fromIntegral rows
    rows = length rs
    ft = buildFrequencyTable att ds
    

gain :: DataSet -> Attribute -> Attribute -> Double
gain ds@(h, rs) att@(attName, attValues) classifier
  = curEntropy - sum [p v * newEntropy v| v <- attValues] 
  where
    p :: AttValue -> Double
    p v = fromIntegral (lookUp v ft) / fromIntegral rows
    rows = length rs
    curEntropy = entropy ds classifier
    newEntropy :: AttValue -> Double
    newEntropy x = entropy (lookUp x partitions) classifier
    partitions = partitionData ds att
    ft = buildFrequencyTable att ds

bestGainAtt :: AttSelector
bestGainAtt ds@(header, _) classifier
  = fst (chooseMax [(att, gain ds att classifier) | att <- header, att /= classifier])
  where
    chooseMax :: [(Attribute, Double)] -> (Attribute, Double)
    chooseMax [x] = x
    chooseMax (x : xs) = if snd x > snd (chooseMax xs)
                         then x
                         else chooseMax xs

--------------------------------------------------------------------

outlook :: Attribute
outlook 
  = ("outlook", ["sunny", "overcast", "rainy"])

temp :: Attribute 
temp 
  = ("temp", ["hot", "mild", "cool"])

humidity :: Attribute 
humidity 
  = ("humidity", ["high", "normal"])

wind :: Attribute 
wind 
  = ("wind", ["windy", "calm"])

result :: Attribute 
result
  = ("result", ["good", "bad"])

fishingData :: DataSet
fishingData
  = (header, table)

header :: Header
table  :: [Row]
header 
  =  [outlook,    temp,   humidity, wind,    result] 
table 
  = [["sunny",    "hot",  "high",   "calm",  "bad" ],
     ["sunny",    "hot",  "high",   "windy", "bad" ],
     ["overcast", "hot",  "high",   "calm",  "good"],
     ["rainy",    "mild", "high",   "calm",  "good"],
     ["rainy",    "cool", "normal", "calm",  "good"],
     ["rainy",    "cool", "normal", "windy", "bad" ],
     ["overcast", "cool", "normal", "windy", "good"],
     ["sunny",    "mild", "high",   "calm",  "bad" ],
     ["sunny",    "cool", "normal", "calm",  "good"],
     ["rainy",    "mild", "normal", "calm",  "good"],
     ["sunny",    "mild", "normal", "windy", "good"],
     ["overcast", "mild", "high",   "windy", "good"],
     ["overcast", "hot",  "normal", "calm",  "good"],
     ["rainy",    "mild", "high",   "windy", "bad" ]]

--
-- This is the same as the above table, but with the result in the second 
-- column...
--
fishingData' :: DataSet
fishingData'
  = (header', table')

header' :: Header
table'  :: [Row]
header' 
  =  [outlook,    result, temp,   humidity, wind] 
table' 
  = [["sunny",    "bad",  "hot",  "high",   "calm"],
     ["sunny",    "bad",  "hot",  "high",   "windy"],
     ["overcast", "good", "hot",  "high",   "calm"],
     ["rainy",    "good", "mild", "high",   "calm"],
     ["rainy",    "good", "cool", "normal", "calm"],
     ["rainy",    "bad",  "cool", "normal", "windy"],
     ["overcast", "good", "cool", "normal", "windy"],
     ["sunny",    "bad",  "mild", "high",   "calm"],
     ["sunny",    "good", "cool", "normal", "calm"],
     ["rainy",    "good", "mild", "normal", "calm"],
     ["sunny",    "good", "mild", "normal", "windy"],
     ["overcast", "good", "mild", "high",   "windy"],
     ["overcast", "good", "hot",  "normal", "calm"],
     ["rainy",    "bad",  "mild", "high",   "windy"]]

fig1 :: DecisionTree
fig1
  = Node "outlook" 
         [("sunny", Node "temp" 
                         [("hot", Leaf "bad"),
                          ("mild",Node "humidity" 
                                       [("high",   Leaf "bad"),
                                        ("normal", Leaf "good")]),
                          ("cool", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "temp" 
                         [("hot", Null),
                          ("mild", Node "humidity" 
                                        [("high",Node "wind" 
                                                      [("windy",  Leaf "bad"),
                                                       ("calm", Leaf "good")]),
                                         ("normal", Leaf "good")]),
                          ("cool", Node "humidity" 
                                        [("high", Null),
                                         ("normal", Node "wind" 
                                                         [("windy",  Leaf "bad"),
                                                          ("calm", Leaf "good")])])])]

fig2 :: DecisionTree
fig2
  = Node "outlook" 
         [("sunny", Node "humidity" 
                         [("high", Leaf "bad"),
                          ("normal", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "wind" 
                         [("windy", Leaf "bad"),
                          ("calm", Leaf "good")])]


outlookPartition :: Partition
outlookPartition
  = [("sunny",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","bad"],["hot","high","windy","bad"],
                   ["mild","high","calm","bad"],["cool","normal","calm","good"],
                   ["mild","normal","windy","good"]])),
     ("overcast",([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","good"],["cool","normal","windy","good"],
                   ["mild","high","windy","good"],["hot","normal","calm","good"]])),
     ("rainy",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["mild","high","calm","good"],["cool","normal","calm","good"],
                   ["cool","normal","windy","bad"],["mild","normal","calm","good"],
                   ["mild","high","windy","bad"]]))]

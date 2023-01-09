module Tries where

import Data.List hiding (insert)
import Data.Bits

import Types
import HashFunctions
import Examples

--------------------------------------------------------------------
-- Part I

-- Use this if you're counting the number of 1s in every
-- four-bit block...
bitTable :: [Int]
bitTable
  = [0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4]

countOnes :: Int -> Int
countOnes 0
  = 0
countOnes n
  = bitTable !! r + countOnes q
  where
    (q, r) = n `divMod` 16
{-
countOnes 0 = 0
countOnes n
  = r + countOnes q
  where
    (q, r) = n `divMod` 2
-}

countOnesFrom :: Int -> Int -> Int
countOnesFrom i n
  = popCount (n .&. (bit i - 1))

getIndex :: Int -> Int -> Int -> Int
getIndex n i s
  = shiftR n (s * i) .&. (bit s - 1)
--  = ((iterate (flip shiftR s) n) !! i) .&. (bit s - 1)
  
-- Pre: the index is less than the length of the list
replace :: Int -> [a] -> a -> [a]
replace i xs y
  = xs' ++ (y : xs'')
  where
    (xs', (_ : xs'')) = splitAt i xs

-- Pre: the index is less than or equal to the length of the list
insertAt :: Int -> a -> [a] -> [a]
insertAt i y xs
  = xs' ++ (y : xs'')
  where
    (xs', xs'') = splitAt i xs

--------------------------------------------------------------------
-- Part II

sumTrie :: (Int -> Int) -> ([Int] -> Int) -> Trie -> Int
sumTrie f g (Leaf xs)
  = g xs
sumTrie f g (Node _ ns)
  = sum (map (sumTrie') ns)
  where
    sumTrie' (Term x)     = f x
    sumTrie' (SubTrie t') = sumTrie f g t'

--
-- If you get the sumTrie function above working you can uncomment
-- these three functions; they may be useful in testing.
--
trieSize :: Trie -> Int
trieSize t
  = sumTrie (const 1) length t

binCount :: Trie -> Int
binCount t
  = sumTrie (const 1) (const 1) t

meanBinSize :: Trie -> Double
meanBinSize t
  = fromIntegral (trieSize t) / fromIntegral (binCount t)

member :: Int -> Hash -> Trie -> Int -> Bool
member v h t s
  = member' t 0
  where
    member' :: Trie -> Int -> Bool
    member' (Leaf xs) l
      = v `elem` xs
    member' (Node bv ns) l
      | testBit bv bvIdx = member'' (ns !! nsIdx)
      | otherwise        = False
      where
        bvIdx = getIndex h l s
        nsIdx = countOnesFrom bvIdx bv

        member'' :: SubNode -> Bool
        member'' (Term x)    = x == v
        member'' (SubTrie t) = member' t (l + 1)

--------------------------------------------------------------------
-- Part III

insert :: HashFun -> Int -> Int -> Int -> Trie -> Trie
insert hf d s v t
  = insert' v t 0
  where
    insert' :: Int -> Trie -> Int -> Trie
    insert' v (Leaf xs) l
      | v `elem` xs = Leaf xs
      | otherwise   = Leaf (v : xs)
    insert' v t l
      | l == d - 1 = Leaf [v]
    insert' v (Node bv ns) l
      | testBit bv bvIdx = Node bv (replace nsIdx ns (insert'' (ns !! nsIdx)))
      | otherwise        = Node (setBit bv bvIdx) (insertAt nsIdx (Term v)  ns)
      where
        bvIdx = getIndex (hf v) l s
        nsIdx = countOnesFrom bvIdx bv
        
        insert'' :: SubNode -> SubNode
        insert'' (Term v')
          | v == v'   = (Term v')
          | otherwise = SubTrie (insert' v t' (l + 1))
          where
            t' = insert' v' empty (l + 1)
        insert'' (SubTrie t)
          = SubTrie (insert' v t (l + 1))

buildTrie :: HashFun -> Int -> Int -> [Int] -> Trie
buildTrie hf d s xs
  = foldr (\x t -> insert hf d s x t) empty xs


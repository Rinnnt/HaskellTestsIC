module SOL where

import Data.List
import Data.Maybe

import Types
import TestData

printF :: Formula -> IO()
printF
  = putStrLn . showF
  where
    showF (Var v)
      = v
    showF (Not f)
      = '!' : showF f
    showF (And f f')
      = "(" ++ showF f ++ " & " ++ showF f' ++ ")"
    showF (Or f f')
      = "(" ++ showF f ++ " | " ++ showF f' ++ ")"

--------------------------------------------------------------------------
-- Part I

-- 1 mark
lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: The item being looked up has a unique binding in the list
lookUp k kvs
  = head [v | (k', v) <- kvs, k == k']

-- 3 marks
vars :: Formula -> [Id]
vars (Var x)    = [x]
vars (Not f)    = vars f
vars (And f f') = (sort . nub) (vars f ++ vars f')
vars (Or f f')  = (sort . nub) (vars f ++ vars f')

-- 1 mark
idMap :: Formula -> IdMap
idMap f
  = zip (vars f) [1..]

--------------------------------------------------------------------------
-- Part II

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c)
  = And (distribute a b) (distribute a c)
distribute (And a b) c
  = And (distribute a c) (distribute b c)
distribute a b
  = Or a b

-- 4 marks
toNNF :: Formula -> NNF
toNNF (Not (Not f))    = toNNF f
toNNF (Not (And f f')) = (Or (toNNF (Not f)) (toNNF (Not f')))
toNNF (Not (Or f f'))  = (And (toNNF (Not f)) (toNNF (Not f')))
toNNF (And f f')       = And (toNNF f) (toNNF f')
toNNF (Or f f')        = Or (toNNF f) (toNNF f')
toNNF x                = x

-- 3 marks
toCNF :: Formula -> CNF
toCNF f
  = toCNF' (toNNF f)
  where
    toCNF' :: NNF -> CNF
    toCNF' (Var x)          = (Var x)
    toCNF' (Not x)          = (Not x)
    toCNF' (And f f')       = (And (toCNF' f) (toCNF' f'))
    toCNF' (Or f f')        = distribute f f'

-- 4 marks
flatten :: CNF -> CNFRep
flatten f
  = flatten' f
  where
    idmap = idMap f
    flatten' :: CNF -> CNFRep
    flatten' (Var x)       = [[lookUp x idmap]]
    flatten' (Not (Var x)) = [[negate (lookUp x idmap)]]
    flatten' (And f f')    = flatten' f ++  flatten' f'
    flatten' (Or f f')     = [concat (flatten' f) ++ concat (flatten' f')]

--------------------------------------------------------------------------
-- Part III

-- 5 marks
propUnits :: CNFRep -> (CNFRep, [Int])
propUnits cnfrep
  | null units = (cnfrep, [])
  | otherwise  = (cnfrep'', unit : prop)
  where
    units = [x | [x] <- cnfrep]
    unit = head units
    cnfrep' = map (\\ [negate unit]) (filter (notElem unit) cnfrep)
    (cnfrep'', prop) = propUnits cnfrep'

-- 4 marks
dp :: CNFRep -> [[Int]]
dp 
  = undefined

--------------------------------------------------------------------------
-- Part IV

-- Bonus 2 marks
allSat :: Formula -> [[(Id, Bool)]]
allSat
  = undefined



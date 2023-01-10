module SC where

import Data.List
import Data.Maybe

import Types
import Examples

---------------------------------------------------------

prims :: [Id]
prims
  = ["+", "-", "*", "<=", "ite"]

lookUp :: Id -> [(Id, a)] -> a
lookUp v env
  = fromMaybe (error ("lookUp failed with search key " ++ v))
              (lookup v env)

---------------------------------------------------------
-- Part I

isFun :: Exp -> Bool
isFun Fun{} = True
isFun _     = False

splitDefs :: [Binding] -> ([Binding], [Binding])
splitDefs =
  partition (isFun . snd)

topLevelFunctions :: Exp -> Int
topLevelFunctions (Let bs _) =
  (length . fst) (splitDefs bs)
topLevelFunctions _ =
  0

---------------------------------------------------------
-- Part II

unionAll :: Eq a => [[a]] -> [a]
unionAll =
  foldl1 (union)

freeVars :: Exp -> [Id]
freeVars (Const n) =
  []
freeVars (Var id) =
  if (elem id prims) then [] else [id]
freeVars (Fun ids e) =
  (freeVars e) \\ ids
freeVars (App e es) =
  unionAll $ map freeVars (e : es)
freeVars (Let bs e) =
  unionAll (map freeVars (e : map snd bs)) \\ (map fst bs)

---------------------------------------------------------
-- Part III

-- Given...
lambdaLift :: Exp -> Exp
lambdaLift e
  = lift (modifyFunctions (buildFVMap e) e)

buildFVMap :: Exp -> [(Id, [Id])]
buildFVMap (Fun ids e) =
  buildFVMap e
buildFVMap (App e es) =
  buildFVMap e ++ concatMap buildFVMap es
buildFVMap (Let bs e) =
  map buildFVMap' fdefs ++ concatMap (buildFVMap . snd) bs ++ buildFVMap e
  where
    (fdefs, _) = splitDefs bs
    fnames     = map fst fdefs
    buildFVMap' :: (Id, Exp) -> (Id, [Id])
    buildFVMap' (fn, e)
      | null fvfs    = (fn, fvs)
      | elem fn sfvs = (fn, sort (union sfvs fvs \\ fnames))
      | otherwise    = (fn, fvs)
      where
        fvs  = freeVars e
        fvfs = intersect fvs fnames
        sfvs = (freeVars . head) [e' | (sfn, e') <- bs, sfn == head fvfs]
buildFVMap _ =
  []

modifyFunctions :: [(Id, [Id])] -> Exp -> Exp
-- Pre: The mapping table contains a binding for every function
-- named in the expression.
modifyFunctions fvmap v@(Var id)
  | elem id fnames = App (Var ('$' : id)) (map Var (lookUp id fvmap))
  where
    fnames = map fst fvmap
modifyFunctions fvmap (App e es) =
  App (modifyFunctions fvmap e) (map (modifyFunctions fvmap) es)
modifyFunctions fvmap (Let bs e) =
  Let (map modify' bs) (modifyFunctions fvmap e)
  where
    modify' :: (Id, Exp) -> (Id, Exp)
    modify' (fn, Fun as e)
      | elem fn fnames = ('$' : fn, Fun as' (modifyFunctions fvmap e))
      where
        as'    = ((lookUp fn fvmap) ++ as)
        fnames = map fst fvmap
    modify' b = b
modifyFunctions fvmap x =
  x
  

-- The default definition here is id.
-- If you implement the above two functions but not this one
-- then lambdaLift above will remove all the free variables
-- in functions; it just won't do any lifting.
lift :: Exp -> Exp
lift e =
  case e' of
    Let bs e'' -> Let (bs ++ ss) e''
    _          -> Let ss e'
  where
    (e', ss) = lift' e
 

-- You may wish to use this...
lift' :: Exp -> (Exp, [Supercombinator])
lift' (Const n) =
  (Const n, [])
lift' (Var id) =
  (Var id, [])
lift' (Fun ids e) =
  ((Fun ids e'), ss)
  where
    (e', ss) = lift' e
lift' (App e es) =
  (App e' e'', ss ++ ss')
  where
    (e', ss) = lift' e
    rs       = map lift' es
    e''      = map fst rs
    ss'      = concatMap snd rs
lift' (Let bs e)
  | null vbs  = (e', fbs ++ ss' ++ ss'')
  | otherwise = (Let vbs e', fbs ++ ss' ++ ss'')
  where
    ns = map fst bs
    rs = map (lift' . snd) bs
    e'' = map fst rs
    ss'' = concatMap snd rs
    bs'' = zip ns e''
    (fbs, vbs) = splitDefs bs''
    (e', ss') = lift' e

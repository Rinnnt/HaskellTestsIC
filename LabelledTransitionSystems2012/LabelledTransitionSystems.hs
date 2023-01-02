import Data.List
import Data.Maybe

type Id = String

type State = Int

type Transition = ((State, State), Id)

type LTS = [Transition]

type Alphabet = [Id]

data Process = STOP | Ref Id | Prefix Id Process | Choice [Process] 
             deriving (Eq, Show)

type ProcessDef = (Id, Process)

type StateMap = [((State, State), State)]

------------------------------------------------------
-- PART I

lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: The item is in the table
lookUp k kvs
  = head [ v | (k', v) <- kvs, k == k']

states :: LTS -> [State]
states
  = nub . concatMap (\((s, s'), _) -> [s, s'])

transitions :: State -> LTS -> [Transition]
transitions s
  = filter ((==s) . fst . fst)

alphabet :: LTS -> Alphabet
alphabet 
  = nub . (map snd)

------------------------------------------------------
-- PART II

actions :: Process -> [Id]
actions STOP         = []
actions (Ref _)      = []
actions (Prefix x p) = x : actions p
actions (Choice ps)  = concatMap actions ps

accepts :: [Id] -> [ProcessDef] -> Bool
--Pre: The first item in the list of process definitions is
--     that of the start process.
accepts as defs@(p : ps)
  = accepts' as (snd p)
  where
    accepts' :: [Id] -> Process -> Bool
    accepts' [] _           = True
    accepts' as (STOP)      = False
    accepts' as (Ref x)     = accepts' as (lookUp x defs)
    accepts' (a : as) (Prefix x p)
      | a == x    = accepts' as p
      | otherwise = False
    accepts' as (Choice ps) = or (map (accepts' as) ps)

------------------------------------------------------
-- PART III

composeTransitions :: Transition -> Transition 
                   -> Alphabet -> Alphabet 
                   -> StateMap 
                   -> [Transition]
--Pre: The first alphabet is that of the LTS from which the first transition is
--     drawn; likewise the second.
--Pre: All (four) pairs of source and target states drawn from the two transitions
--     are contained in the given StateMap.
composeTransitions ((s, t), a) ((s', t'), a') alph alph' stateMap
  | a == a'           = [((s1, s4), a)]
  | aConst && a'Const = []
  | a'Const           = [((s1, s3), a)]
  | aConst            = [((s1, s2), a')]
  | otherwise         = [((s1, s2), a'), ((s1, s3), a)]
  where
    s1      = lookUp (s, s') stateMap
    s2      = lookUp (s, t') stateMap
    s3      = lookUp (t, s') stateMap
    s4      = lookUp (t, t') stateMap
    aConst  = a `elem` alph'
    a'Const = a' `elem` alph

pruneTransitions :: [Transition] -> LTS
pruneTransitions ts
  = visit 0 []
  where
    visit :: State -> [State] -> [Transition]
    visit s visited
      | s `elem` visited = []
      | otherwise        = ns ++ concatMap ((flip visit (s : visited)) . snd . fst) ns
      where
        ns = transitions s ts

------------------------------------------------------
-- PART IV

compose :: LTS -> LTS -> LTS
compose lts lts'
  = (nub . pruneTransitions) (concatMap newTransitions nss)
  where
    alph     = "$'" : alphabet lts
    ss       = states lts
    alph'    = "$" : alphabet lts'
    ss'      = states lts'
    nss      = [(s, s') | s <- ss, s' <- ss']
    stateMap = map (\(s, s') -> ((s, s'), s * (length ss') + s')) nss

    newTransitions :: (State, State) -> [Transition]
    newTransitions (s, s')
      = concatMap (\(t, t') -> composeTransitions t t' alph alph' stateMap) nts
      where
        ts   = transitions s lts
        ets  = [((s, 0), "$")]
        ts'  = transitions s' lts'
        ets' = [((s', 0), "$'")]
        nts  = [(t, t') | t <- if (null ts) then ets else ts,
                          t' <- if (null ts') then ets' else ts']

------------------------------------------------------
-- PART V

buildLTS :: [ProcessDef] -> LTS
-- Pre: All process references (Ref constructor) have a corresponding
--      definition in the list of ProcessDefs.
buildLTS 
  = undefined

------------------------------------------------------
-- Sample process definitions...

vendor, clock, play, maker, user, p, q, switch, off, on :: ProcessDef

vendor 
  = ("VENDOR", Choice [Prefix "red"  (Prefix "coffee" (Ref "VENDOR")),
                       Prefix "blue" (Prefix "tea" (Ref "VENDOR")),
                       Prefix "off" STOP])

clock 
  = ("CLOCK", Prefix "tick" (Prefix "tock" (Ref "CLOCK")))

play 
  = ("PLAY", Choice [Prefix "think" (Prefix "move" (Ref "PLAY")), 
                     Prefix "end" STOP])

maker 
  = ("MAKER", Prefix "make" (Prefix "ready" (Ref "MAKER")))

user  
  = ("USER",  Prefix "ready" (Prefix "use" (Ref "USER")))

p = ("P", Prefix "a" (Prefix "b" (Prefix "c" STOP)))

q = ("Q",  Prefix "d" (Prefix "c" (Prefix "b" (Ref "Q"))))

switch 
  = ("SWITCH", Ref "OFF")

off 
  = ("OFF", Choice [Prefix "on" (Ref "ON")])

on  
  = ("ON",  Choice [Prefix "off" (Ref "OFF")])

------------------------------------------------------
-- Sample LTSs...

vendorLTS, clockLTS, playLTS, clockPlayLTS, makerLTS, userLTS, makerUserLTS, 
  pLTS, qLTS, pqLTS, switchLTS :: LTS

vendorLTS 
  = [((0,1),"off"),((0,2),"blue"),((0,3),"red"),((2,0),"tea"),((3,0),"coffee")]

clockLTS 
  = [((0,1),"tick"),((1,0),"tock")]

playLTS 
  = [((0,1),"end"),((0,2),"think"),((2,0),"move")]

clockPlayLTS 
  = [((0,1),"end"),((1,4),"tick"),((4,1),"tock"),((0,3),"tick"),
     ((3,4),"end"),((3,0),"tock"),((3,5),"think"),((5,3),"move"),
     ((5,2),"tock"),((2,0),"move"),((2,5),"tick"),((0,2),"think")]

makerLTS 
  = [((0,1),"make"),((1,0),"ready")]

userLTS 
  = [((0,1),"ready"),((1,0),"use")]

makerUserLTS 
  = [((0,2),"make"),((2,1),"ready"),((1,0),"use"),((1,3),"make"),((3,2),"use")]

pLTS 
  = [((0,1),"a"),((1,2),"b"),((2,3),"c")]

qLTS 
  = [((0,1),"d"),((1,2),"c"),((2,0),"b")]

pqLTS 
  = [((0,1),"d"),((1,4),"a"),((0,3),"a"),((3,4),"d")]

switchLTS 
  = [((0,1),"on"),((1,0),"off")]


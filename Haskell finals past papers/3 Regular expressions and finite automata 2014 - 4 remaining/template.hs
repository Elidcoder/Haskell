import Data.Maybe
import Data.List
import Debug.Trace
data RE = Null   |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)

type State = Int

data Label = C Char | Eps
           deriving (Eq, Ord, Show)

type Transition = (State, State, Label)

type Automaton = (State, [State], [Transition])

--------------------------------------------------------
-- showRE - this may be useful for testing

showRE :: RE -> String
showRE (Seq re re')
  = showRE re ++ showRE re'
showRE (Alt re re')
  = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re)
  = showRE' re ++ "*"
showRE (Plus re)
  = showRE' re ++ "+"
showRE (Opt re)
  =  showRE' re ++ "?"
showRE re
  = showRE' re

showRE' Null
  = ""
showRE' (Term c)
  = [c]
showRE' (Alt re re')
  = showRE (Alt re re')
showRE' re
  = "(" ++ showRE re ++ ")"

--------------------------------------------------------
--My funcs
get1st :: (a, b, c) -> a
get1st (a, _, _) = a

get2nd :: (a, b, c) -> b
get2nd (_, b, _) = b

get3rd :: (a, b, c) -> c
get3rd (_, _, c) = c
-- Part I

lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: There is exactly one occurrence of the item being looked up.
lookUp searchItem
  = snd . head . dropWhile ((/= searchItem) . fst)

simplify :: RE -> RE
simplify (Seq re1 re2) = Seq (simplify re1) (simplify re2)
simplify (Alt re1 re2) = Alt (simplify re1) (simplify re2)
simplify (Rep re)      = Rep (simplify re)
simplify (Opt re)      = Alt (simplify re) Null
simplify (Plus re)     = Seq (re') (Rep re')
  where
    re' = simplify re
simplify re            = re

--------------------------------------------------------
-- Part II

startState :: Automaton -> State
startState = get1st

terminalStates :: Automaton -> [State]
terminalStates = get2nd

transitions :: Automaton -> [Transition]
transitions = get3rd

isTerminal :: State -> Automaton -> Bool
isTerminal inputState = (elem inputState) . terminalStates

transitionsFrom :: State -> Automaton -> [Transition]
transitionsFrom startingState
  = (filter (\(a, _, _) -> a == startingState)) . transitions

labels :: [Transition] -> [Label]
labels transitionList
  = nub [c | (a, b, c) <- transitionList, c /= Eps]

accepts :: Automaton -> String -> Bool
accepts automaton@(startingState,  termStates, transitionList) inputString
  = accepts' startingState inputString
  where
    accepts' :: State -> String -> Bool
    accepts' currentState "" 
      | currentState `elem` termStates = True
      | any (flip accepts' "") [ end | (start, end, label) <- transitionList, label == Eps, start == currentState] = True
      | otherwise = False
    accepts' currentState currentString = any (flip trace currentString) (filter ((==currentState) . get1st) transitionList)
    
    trace :: Transition -> String -> Bool
    trace (start, end, label) matchString 
      | label == Eps = accepts' end matchString
      | (chr1: chrs) <- matchString, C chr1 == label = accepts' end chrs
      | otherwise = False   
    
    -- accepts' (Seq re1 re2) remainingStr = 
    -- accepts' (Alt re1 re2) remainingStr = 
    -- accepts' (Rep re) remainingStr = 
    -- accepts' (Opt re) remainingStr =      
    -- accepts' (Plus re) remainingStr = 
    -- accepts' re remainingStr =          
{-
Null   |
  Term Char |
  Seq RE RE |
  Alt RE RE |
  Rep RE    |
  Plus RE   |
  Opt RE

data Label = C Char | Eps
           deriving (Eq, Ord, Show)

type Transition = (State, State, Label)

type Automaton = (State, [State], [Transition])
-}
      -- | currentState `elem` termStates = True
      -- | (Seq re1 re2) <- currentState = 
      -- | (Alt re1 re2) <- currentState = 
      -- | (Rep re) <- currentState = 
      -- | (Opt re) <- currentState = 
      -- | (Plus re) <- currentState = 
      -- | (Term character) <- currentState = 
      -- | otherwise = False
--------------------------------------------------------
-- Part III

makeNDA :: RE -> Automaton
makeNDA re
  = (1, [2], sort transitions)
  where
    (transitions, k) = make (simplify re) 1 2 3
-- (_, int) where int is the next abailable id from below call
make :: RE -> Int -> Int -> Int -> ([Transition], Int)
make Null startM endN nextAvailableIdK
  = ([(startM, endN, Eps)], nextAvailableIdK)
make (Term chr) startM endN nextAvailableIdK
  = ([(startM, endN, C chr)], nextAvailableIdK)
make (Seq re1 re2) startM endN nextAvailableIdK
  = (bonusTransition: transitions1 ++ transitions2, nextAvailableId') 
  where
    nda1@(transitions1, nextAvailableId) = make re1 startM nextAvailableIdK (nextAvailableIdK + 2)
    nda2@(transitions2, nextAvailableId') = make re2 (nextAvailableIdK + 1) endN nextAvailableId
    bonusTransition = (nextAvailableIdK, nextAvailableIdK+1, Eps)
make (Alt re1 re2) startM endN nextAvailableIdK
  = (bonusTransitions ++ transitions1 ++ transitions2, nextAvailableId')
  where
    bonusTransition11 = (startM, nextAvailableIdK  , Eps)
    bonusTransition12 = (nextAvailableIdK+1, endN, Eps)
    bonusTransition21 = (startM, nextAvailableIdK+2, Eps)
    bonusTransition22 = (nextAvailableIdK+3, endN, Eps)
    bonusTransitions = [bonusTransition11, bonusTransition12, bonusTransition21, bonusTransition22]
    nda1@(transitions1, nextAvailableId) = make re1 nextAvailableIdK (nextAvailableIdK + 1) (nextAvailableIdK + 4)
    nda2@(transitions2, nextAvailableId') = make re2 (nextAvailableIdK + 2) (nextAvailableIdK + 3) nextAvailableId
make (Rep re) startM endN nextAvailableIdK
  = (bonusTransitions ++ transitions1, nextAvailableId)
  where
    bonusTransition11 = (startM, nextAvailableIdK  , Eps)
    bonusTransition12 = (nextAvailableIdK+1, nextAvailableIdK  , Eps)
    bonusTransition21 = (startM, endN, Eps)
    bonusTransition22 = (nextAvailableIdK+1, endN, Eps)
    bonusTransitions = [bonusTransition11, bonusTransition12, bonusTransition21, bonusTransition22]
    nda1@(transitions1, nextAvailableId) = make re nextAvailableIdK (nextAvailableIdK + 1) (nextAvailableIdK + 2)
--------------------------------------------------------
-- Part IV

type MetaState = [State]

type MetaTransition = (MetaState, MetaState, Label)
getMeta :: [Transition] -> [State]
getMeta = sort . nub . map get1st
getFrontier :: State -> Automaton -> [Transition]
getFrontier currentState automaton@(start, termStates, autTransitions)
  = concatMap myF (filter ((== currentState) . get1st) autTransitions)
  where
    myF t@(a, b, Eps)
      | b `elem` termStates = [(b, b, Eps)] 
      | otherwise = getFrontier b automaton
    myF t = [t]
  
groupTransitions :: [Transition] -> [(Label, [State])]
groupTransitions transList
  = map (\x -> (x, [b | (_,b,_) <- filter ((== x) . get3rd) transList])) labelGroups
  where
    labelGroups = labels transList

makeDA :: Automaton -> Automaton
-- Pre: Any cycle in the NDA must include at least one non-Eps transition
makeDA 
  = undefined

--------------------------------------------------------
-- Test cases

reFigure, re1, re2, re3, re4, re5 :: RE
reFigure
  = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1
  = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2
  = Seq (Term 'x') (Rep (Term '\''))
re3
  = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4
  = Seq (Alt (Term 'a') Null) (Term 'a')
re5
  = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))

nd, nd' :: Automaton
nd = (1,[4],[(1,2,C 'a'),(1,3,C 'b'),(2,3,Eps),(2,4,C 'c')])

nd' = (1,[4],[(1,2,Eps),(1,3,C 'a'),(2,4,C 'a'),(2,4,C 'b'),
              (3,4,C 'b'),(3,4,Eps)])

da :: Automaton
da = (0,[3],[(0,1,C 'a'),(0,2,C 'a'),(0,2,C 'b'),(1,2,C 'a'),
             (1,3,C 'b'),(2,2,C 'a'),(2,1,C 'a'),(2,3,C 'b')])

re :: RE
re = Seq (Alt (Term 'a') (Term 'b')) (Seq (Rep (Term 'a')) (Term 'b'))

ndaFigure, nda1, nda2, nda3, nda4, nda5 :: Automaton
daFigure, da1, da2, da3, da4, da5 :: Automaton
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])
daFigure
  = (1,[2],
     [(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),
            (5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),
            (10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1
  = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2
  = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2
  = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3
  = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3
  = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4
  = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5
  = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])

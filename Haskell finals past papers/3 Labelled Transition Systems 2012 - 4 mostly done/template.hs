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
lookUp val = snd . head . dropWhile ((/= val) . fst) 

states :: LTS -> [State]
states = nub . concatMap (\((a,b), c) -> [a,b])

transitions :: State -> LTS -> [Transition]
transitions startState = filter ((== startState) . fst . fst)

alphabet :: LTS -> Alphabet
alphabet = nub . snd . unzip

------------------------------------------------------
-- PART II

actions :: Process -> [Id]
actions (Choice processes) = concatMap actions processes
actions (Prefix id subProcess) = id : actions subProcess
actions _ = []

accepts :: [Id] -> [ProcessDef] -> Bool
--Pre: The first item in the list of process definitions is
--     that of the start process.
accepts possibleTr allProcesses@((_, initialProcess): _) = accepts' [] initialProcess
  where
    checkTr = (==) possibleTr 
    -- unnecessary definition of check as it could be defined within fullCheck 
    --however this makes it clearer and ensures the number of characters < 80
    -- & inefficiency should be compiled away so there is no performance hit
    checkLen = flip (<) (length possibleTr) . length
    fullCheck inputTr = ((||) (checkTr inputTr)) . ((&&) (checkLen inputTr))
    accepts' :: [String] -> Process -> Bool
    accepts' currentTr (STOP) = (checkTr currentTr)
    accepts' currentTr (Choice processOptions) = 
        fullCheck currentTr (any (accepts' currentTr) processOptions)
    accepts' currentTr (Ref id) = 
        fullCheck currentTr (
          accepts' 
            currentTr 
            ((snd . head . filter ((== id) . fst)) allProcesses)
        )
    accepts' currentTr (Prefix id process) =
        fullCheck currentTr (accepts' (currentTr ++ [id]) process)

------------------------------------------------------
-- PART III

--composeTransitions :: Transition -> Transition 
--                   -> Alphabet -> Alphabet 
--                   -> StateMap 
--                   -> [Transition]
--Pre: The first alphabet is that of the LTS from which the first transition is
--     drawn; likewise the second.
--Pre: All (four) pairs of source and target states drawn from the two transitions
--     are contained in the given StateMap.
composeTransitions :: Transition -> Transition -> Alphabet -> Alphabet -> StateMap -> [Transition]
composeTransitions ((initialStateL, finalStateL), idL) ((initialStateR, finalStateR), idR) alphabetL alphabetR stateMap
  | idL == idR = [((i' (initialStateL,initialStateR), i' (finalStateL,finalStateR)), idL)]
  | idLElAlphR && idRElAlphL = []
  | idLElAlphR = [((i' (initialStateL,initialStateR), i' (initialStateL,finalStateR)), idR)]
  | idRElAlphL = [((i' (initialStateL,initialStateR), i' (finalStateL,initialStateR)), idL)]
  | otherwise = [((i' (initialStateL,initialStateR), i' (initialStateL,finalStateR)), idR), ((i' (initialStateL,initialStateR), i' (finalStateL,initialStateR)), idL)]
  where 
    idLElAlphR = idL `elem` alphabetR
    idRElAlphL = idR `elem` alphabetL
    i' = flip lookUp stateMap 

pruneTransitions :: [Transition] -> LTS
pruneTransitions initialTransitions = initialTransitions \\ (removeReachable [0] initialTransitions)
  where
    removeReachable :: [State] -> [Transition] -> [Transition]
    removeReachable nodeList transitionList 
      | null possibleTransitions = transitionList
      | otherwise = removeReachable connectingNodes impossibleTransitions
        where
          (possibleTransitions, impossibleTransitions) = partition ((`elem` nodeList) . fst .fst) transitionList
          connectingNodes = map (snd . fst) possibleTransitions

------------------------------------------------------
-- PART IV

compose :: LTS -> LTS -> LTS
compose ltsLeft ltsRight = finalTransitions
  where
    leftAlphabet = alphabet ltsLeft
    rightAlphabet = alphabet ltsRight
    setOfStatePairs = [(x, y) | x <- states ltsLeft, y <- states ltsRight] 
    i' = zip setOfStatePairs [0..]
    finalTransitions = pruneTransitions ( concat [(composeTransitions l' r' lAlph rAlph i') | (l,r) <- setOfStatePairs, l' <- transitions l ltsLeft, r' <- transitions r ltsRight, let lAlph = alphabet (transitions l ltsLeft), let rAlph = alphabet (transitions r ltsRight)])
--untested, 2nd part of composition undone 
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

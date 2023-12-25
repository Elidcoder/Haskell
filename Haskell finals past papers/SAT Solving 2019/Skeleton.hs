module SOL where

import Data.List
import Data.Maybe
import Debug.Trace
import Types
import TestData


myFunc :: [[Int]] -> [[Int]]
myFunc = filter isSingle 
  where
    isSingle (a: []) = True
    isSingle _       = False

alphabet :: String
alphabet = "Xabcdefghijklmnopqrstuvwxyz"

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
lookUp item list = b'
  where 
    ((_, b'): _) = dropWhile ((/= item) . fst) list

-- 3 marks
vars :: Formula -> [Id]
vars = sort . nub . vars'
  where
    vars' (And f1 f2) = vars' f1 ++ vars' f2
    vars' (Or f1 f2)  = vars' f1 ++ vars' f2
    vars' (Not f1)    = vars' f1 
    vars' (Var id)    = [id]

-- 1 mark
idMap :: Formula -> IdMap
idMap = (`zip` [1..]) . vars  


--------------------------------------------------------------------------
-- Part II

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c) = And (distribute a b) (distribute a c)
distribute (And a b) c = And (distribute a c) (distribute b c)
distribute a b         = Or a b

-- 4 marks
toNNF :: Formula -> NNF
toNNF (Not (Not formula)) = toNNF formula
toNNF (Not (And f1 f2))   = Or (toNNF (Not f1)) (toNNF (Not f2))
toNNF (Not (Or f1 f2))    = And (toNNF (Not f1)) (toNNF (Not f2))
toNNF (And f1 f2)         = And (toNNF f1) (toNNF f2)
toNNF (Or f1 f2)          = Or (toNNF f1) (toNNF f2)
toNNF (Not f1)            = Not (toNNF f1)
toNNF f                   = f
-- 3 marks
toCNF :: Formula -> CNF
toCNF = toCNF' . toNNF
  where 
    toCNF' :: Formula -> CNF
    toCNF' (Or f1 f2) = distribute f1 f2
    toCNF' f          = f

-- 4 marks
flatten :: CNF -> CNFRep
flatten formula = (map flatten' . unPackA) formula
  where 
    myIdMap = idMap formula
    unPackA (And f1 f2) = (unPackA f1) ++ (unPackA f2)
    unPackA f = [f]
    flatten' (Or a b)  = (flatten' a) ++ (flatten' b)
    flatten' (Not (Var id)) = [ - (lookUp id myIdMap)]
    flatten' (Var id)  = [lookUp id myIdMap]

--------------------------------------------------------------------------
-- Part III

-- 5 marks
propUnits :: CNFRep -> (CNFRep, [Int])
propUnits clauseList  
  | (noSingles == clauseList) = (clauseList, [])
  | otherwise = (finalClauseList, removedValues ++ otherRemovedSingles)
    where
      (noSingles, removedValues) = removeSingles clauseList
      propNoSignals 
        = [subList | x <- noSingles, let negativeX = (map negate x),  let subList = if ((intersect removedValues negativeX) == negativeX) then ([]) else ([y | y <- x, not ( -y `elem` removedValues)]), null ( intersect removedValues x)]
      (finalClauseList, otherRemovedSingles) = propUnits propNoSignals
      
      removeSingles :: CNFRep -> (CNFRep, [Int])
      removeSingles [] = ([], [])
      removeSingles (x: xs) 
        | (x1: []) <- x, not ((-x1) `elem` nextRemoved) = (nextList, x1: nextRemoved)
        | otherwise = (x: nextList, nextRemoved)
          where
            (nextList, nextRemoved) = removeSingles xs

-- 4 marks
dp :: CNFRep -> [[Int]]
dp formula
  | null cnfFormula' = [singleVals]
  | [] `elem` cnfFormula' = []
  | otherwise = 
    let
      ((setVariable : xs):ys) = cnfFormula'
      option1     = [setVariable] : formulaToPass
      option2     = [- setVariable] : formulaToPass
    in
      
      ((dp option1) ++ (dp option2))
    where
      (cnfFormula', singleVals) = propUnits formula
      usefullSingles = map (:[]) singleVals
      formulaToPass = usefullSingles ++ cnfFormula'


--------------------------------------------------------------------------
-- Part IV

-- Bonus 2 marks
allSat :: Formula -> [[(Id, Bool)]]
allSat formula
  = [[if (y < 0) then ((lookUp' (-y), False)) else ((lookUp' y, True)) | y <- x] | x <- sols]
  where
    sols = e1 . dp . flatten . toCNF formula
    n      = length (vars formula)
    values' = [1..n]
    
    e1 :: [[Int]] -> [[Int]]
    e1 []     = []
    e1 (x:xs) 
      | null (drop (n - 1) x) = (e1' x missingValues) ++ evalRest
      | otherwise = x : evalRest
        where
          missingValues = [i | i <- values', null (intersect [i, -i] x)]
          evalRest      = e1 xs
          e1' :: [Int] -> [Int] -> [[Int]]
          e1' listNumb []      = [listNumb]
          e1' listNumb (y: ys) = (e1' (y:listNumb) ys) ++ (e1' ((-y):listNumb) ys)

    theIDMap = idMap formula
    lookUp' :: Int -> Id
    lookUp' item  = a'
      where 
        ((a', _): _) = dropWhile ((/= item) . snd) theIDMap

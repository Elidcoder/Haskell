module Alloc where

import Data.Maybe
import Data.List

import Types
import Examples

------------------------------------------------------
--
-- Part I
--
count :: Eq a => a -> [a] -> Int
count val = foldl (flip ((+) . (fromEnum . (== val)))) 0 

degrees :: Eq a => Graph a -> [(a, Int)]
degrees (values, edges)
  = map (\x -> (x, length (filter (\(a,b) -> a == x || b == x ) edges))) values

neighbours :: Eq a => a -> Graph a -> [a]
neighbours val (values, edges)
  = [if (a == val) then (b) else (a) | (a, b) <- edges, let a' = (a == val), a' || (b == val)]

removeNode :: Eq a => a -> Graph a -> Graph a
removeNode val (values, edges)
  = (delete val values, filter (\(a, b) -> (a /= val) && (b /= val)) edges)

------------------------------------------------------
--
-- Part II
--
colourGraph :: (Ord a, Show a) => Int -> Graph a -> Colouring a
colourGraph maxColour ([],_)       = []
colourGraph maxColour (values, []) = map (\x -> (x, 1)) values
colourGraph maxColour curGraph@(values, edges) 
  | null availableColours = (curNode, 0): nextColored
  | otherwise, (minColour: colours) <- availableColours = (curNode, minColour): nextColored
    where
      (curNode: nodes) = sort values 
      nextColored = colourGraph maxColour (removeNode curNode curGraph)
      curNeighbours = neighbours curNode curGraph
      availableColours = [1..maxColour] \\ [ b | (a,b) <- nextColored, a `elem` curNeighbours]
------------------------------------------------------
--
-- Part III
--
buildIdMap :: Colouring Id -> IdMap
buildIdMap list
  = ("return", "return"): map buildIdMap' list
  where 
    buildIdMap' (a, 0) = (a, a)
    buildIdMap' (a, b) = (a, "R" ++ show b)

buildArgAssignments :: [Id] -> IdMap -> [Statement]
buildArgAssignments vars iMap = map (\x -> Assign (lookUp x iMap) (Var x)) vars

renameExp :: Exp -> IdMap -> Exp
-- Pre: A precondition is that every variable referenced in 
-- the expression is in the idMap. 
renameExp initialExpr idMap = renameExp' initialExpr
  where
    renameExp' (Var x)         = Var (lookUp x idMap)
    renameExp' (Apply op exp1 exp2) = Apply op (renameExp' exp1) (renameExp' exp2)
    renameExp' otherExpr          = otherExpr

renameBlock :: Block -> IdMap -> Block
-- Pre: A precondition is that every variable referenced in 
-- the block is in the idMap. 
renameBlock inputBlock idMap = map renameStatement inputBlock
  where
    renameStatement :: Statement -> Statement
    renameStatement (Assign id exp) = Assign lhs rhs
        where
            lhs = lookUp id idMap
            rhs = renameExp exp idMap
    renameStatement (If exp block1 block2) = If (renameExp exp idMap) (parseSubblock block1) (parseSubblock block2)
    renameStatement (While exp block) = While (renameExp exp idMap) (parseSubblock block)
    
    parseSubblock :: Block -> Block
    parseSubblock block = [renamed | x <- block, let renamed = renameStatement x, isGoodAssign renamed]

    isGoodAssign (Assign id (Var exp')) = not (id == exp')
    isGoodAssign _ = True

renameFun :: Function -> IdMap -> Function
renameFun (f, as, b) idMap
  = (f, as, buildArgAssignments as idMap ++ renameBlock b idMap)

-----------------------------------------------------
--
-- Part IV
--
buildIG :: [[Id]] -> IG
buildIG liveVarSets = (vars, simultaneousLives) 
  where
    pairs = filter pairsPlus liveVarSets
    simultaneousLives = nub ( [(x,y) | pairs' <- pairs, x <- pairs', y <- pairs', x > y])
    vars = nub (concatMap (\(a, b) -> [a,b]) simultaneousLives)
    pairsPlus []     = False
    pairsPlus (x:[]) = False
    pairsPlus _      = True

-----------------------------------------------------
--
-- Part V
--
liveVars :: CFG -> [[Id]]
liveVars 
  = undefined

buildCFG :: Function -> CFG
buildCFG 
  = undefined
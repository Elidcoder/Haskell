import Data.List

type Index = Int

data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)

type Env = [(Index, Bool)]

type NodeId = Int

type BDDNode =  (NodeId, (Index, NodeId, NodeId))

type BDD = (NodeId, [BDDNode])

------------------------------------------------------
-- PART I

-- Pre: The item is in the given table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp value
  = snd . head . (filter ((value == ) . fst))

checkSat :: BDD -> Env -> Bool
checkSat (startID, bddNodes) environment
  = checkSat' startID
  where
    checkSat' :: NodeId -> Bool
    checkSat' 1 = True
    checkSat' 0 = False
    checkSat' n
      | thisValueTrue = checkSat' idR
      | otherwise = checkSat' idL
      where
        (index, idL, idR) = lookUp n bddNodes
        thisValueTrue = lookUp index environment

sat :: BDD -> [[(Index, Bool)]]
sat bdd@(_, bddNodes)
  = filter (checkSat bdd) options 
  where
    genOptions :: [Int] -> [[(Int, Bool)]]
    genOptions [] = [[]]
    genOptions (x: xs) =  (concat [[(x, True): fg', (x, False): fg'] | fg' <- futureGen])
      where 
        futureGen = genOptions xs
    vars = nub (map (\(_, (x, _, _)) -> x) bddNodes)
    options = genOptions vars
------------------------------------------------------
-- PART II

simplify :: BExp -> BExp
simplify (Not (Prim False))             = Prim True
simplify (Not (Prim True))              = Prim False
simplify (Or (Prim False) (Prim False)) = Prim False
simplify (Or (Prim _) (Prim _))         = Prim True
simplify (And (Prim True) (Prim True))  = Prim True
simplify (And (Prim _) (Prim _))        = Prim False
simplify exp                            = exp

restrict :: BExp -> Index -> Bool -> BExp
restrict expr@(IdRef index) index' bool 
  | index == index' = Prim bool
  | otherwise       = expr
restrict (Not bExp) index bool = simplify (Not (restrict bExp index bool)) 
restrict (And bExp1 bExp2) index bool = simplify (And (restrict bExp1 index bool) (restrict bExp2 index bool)) 
restrict (Or bExp1 bExp2) index bool = simplify (Or (restrict bExp1 index bool) (restrict bExp2 index bool)) 
restrict bExp _ _ = bExp 

------------------------------------------------------
-- PART III

-- Pre: Each variable index in the BExp appears exactly once
--     in the Index list; there are no other elements
-- The question suggests the following definition (in terms of buildBDD')
-- but you are free to implement the function differently if you wish.
buildBDD :: BExp -> [Index] -> BDD
buildBDD e is'
  = buildBDD' e 2 is'

-- Potential helper function for buildBDD which you are free
-- to define/modify/ignore/delete/embed as you see fit.
buildBDD' :: BExp -> NodeId -> [Index] -> BDD
buildBDD' e' nID []
  | Prim e'' <- e', e'' = (1, []) 
  | otherwise = (0, [])
buildBDD' e' nodeID (x: xs) = (nodeID,(valuesL ++ valuesR ++ [thisNode]))
  where
    (indexL, valuesL) = buildBDD' (restrict e' x False)  (2 * nodeID)     xs
    (indexR, valuesR) = buildBDD' (restrict e' x True) (2 * nodeID + 1) xs
    thisNode =  (nodeID, (x, indexL, indexR))

------------------------------------------------------
-- PART IV

-- Pre: Each variable index in the BExp appears exactly once
--      in the Index list; there are no other elements
buildROBDD :: BExp -> [Index] -> BDD
buildROBDD e is'
  = buildROBDD' e 2 is'

-- Potential helper function for buildROBDD which you are free
-- to define/modify/ignore/delete/embed as you see fit.
buildROBDD' :: BExp -> NodeId -> [Index] -> BDD
buildROBDD' e' nID []
  | Prim e'' <- e', e'' = (1, []) 
  | otherwise = (0, [])
buildROBDD' e' nodeID (x: xs) 
  | indexL == indexR = (nodeID,(valuesL ++ [(nodeID, (x, indexL, indexR))]))
  | otherwise = (nodeID,(valuesL ++ valuesR ++ [thisNode]))
  where
    (indexL, valuesL) = buildROBDD' (restrict e' x False)  (2 * nodeID)   xs
    (indexR, valuesR) = buildROBDD' (restrict e' x True) (2 * nodeID + 1) xs
    thisNode =  (nodeID, (x, indexL, indexR))

------------------------------------------------------
-- Examples for testing...

b1, b2, b3, b4, b5, b6, b7, b8 :: BExp
b1 = Prim False
b2 = Not (And (IdRef 1) (Or (Prim False) (IdRef 2)))
b3 = And (IdRef 1) (Prim True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
bdd3 = (5,[(5,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])


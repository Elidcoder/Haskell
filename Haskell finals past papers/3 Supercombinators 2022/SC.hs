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
isFun (Fun _ _) = True
isFun _         = False

splitDefs :: [Binding] -> ([Binding], [Binding])
splitDefs = partition (isFun . snd)

topLevelFunctions :: Exp -> Int
topLevelFunctions (Let bindings _)
  = (length . fst . splitDefs) bindings
topLevelFunctions _ = 0

---------------------------------------------------------
-- Part II

unionAll :: Eq a => [[a]] -> [a]
unionAll
  = foldr1 union

freeVars :: Exp -> [Id]
freeVars (Const numb) = []
freeVars (Var id)  
  | id `elem` prims = []
  | otherwise = [id]
freeVars (Fun ids exp) = (freeVars exp) \\ ids
freeVars (App exp inputExprs) = unionAll (map freeVars (exp: inputExprs))
freeVars (Let bindings exp) = unionAll (map freeVars (exp: (map snd bindings))) \\ (map fst bindings) 
{-
--
-- Note: A Fun expression can only appear in a Let binding, i.e.
-- there are no anonymous functions. For example,
--
--   App (Fun ["x"] (Var "x")) [Const 3]
-- 
-- is not well formed for the purposes of the exercise.
--

data Exp = Const Int | 
           Var Id | 
           Fun [Id] Exp |
           App Exp [Exp] |
           Let [Binding] Exp 
         deriving (Eq, Show)

type Id = String

type Binding = (Id, Exp)

type Supercombinator = Binding
-}
---------------------------------------------------------
-- Part III

-- Given...
lambdaLift :: Exp -> Exp
lambdaLift e
  = lift (modifyFunctions (buildFVMap e) e)

buildFVMap :: Exp -> [(Id, [Id])]
buildFVMap (Const _ ) = []
buildFVMap (Var _ ) = []
buildFVMap (Fun _ exp ) = buildFVMap exp
buildFVMap (App exp exps ) = concatMap buildFVMap (exp:exps)
buildFVMap (Let bindings exp) = [(id, overallFreeVars) | (id, expr) <- bindings, isFun expr] ++ (concatMap buildFVMap (exp: (map snd bindings)))
  where 
    functions = filter (isFun . snd) bindings
    overallFreeVars = unionAll (map (freeVars . snd) functions) \\ (map fst functions) 

modifyFunctions :: [(Id, [Id])] -> Exp -> Exp
-- Pre: The mapping table contains a binding for every function
-- named in the expression.
modifyFunctions mappingTable initialExpr
  = modifyFunctions' initialExpr
  where 
    modifyFunctions' :: Exp -> Exp
    modifyFunctions' (Let bindings exp) = Let (map rewrite bindings) (modifyFunctions' exp)
      where
        rewrite (fName, (Fun as e)) = ('$': fName, Fun ((lookUp fName mappingTable) ++ as) (modifyFunctions' e))
        rewrite f = f
    modifyFunctions' v@(Var fName) 
      | (Just []) <- freeVars = Var (newFName)
      | (Just vars) <- freeVars = App (Var newFName) (map Var vars)
      | otherwise = v
        where
          newFName = '$': fName
          freeVars = (lookup fName mappingTable) 
    modifyFunctions' c@(Const _) = c
    modifyFunctions' (App exp exps) = App (modifyFunctions' exp) (map modifyFunctions' exps)

-- The default definition here is id.
-- If you implement the above two functions but not this one
-- then lambdaLift above will remove all the free variables
-- in functions; it just won't do any lifting.
lift :: Exp -> Exp
lift 
  = id

-- You may wish to use this...
lift' :: Exp -> (Exp, [Supercombinator])
lift' (Let bindings exp)
  = undefined
  where
    (supercombinators, otherBinds) = partition (/ISSUPERCOMBINATOR/)

{-
data Exp = Const Int | 
    Var Id | 
    Fun [Id] Exp |
    App Exp [Exp] |
    Let [Binding] Exp 
    deriving (Eq, Show)
    -}
data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Show)

------------------------------------------------------

isPrefix :: String -> String -> Bool
isPrefix sub = (==) sub . take (length sub) 


removePrefix :: String -> String -> String
removePrefix  
--Pre: s is a prefix of s'
  = drop . length 

suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes a  = a : suffixes (tail a)

isSubstring :: String -> String -> Bool
isSubstring sub =  any (isPrefix sub) . suffixes

findSubstrings :: String -> String -> [Int]
findSubstrings sub full = [b | (a,b) <- (zip (suffixes full) [0..]), isPrefix sub a ]

------------------------------------------------------

getIndices :: SuffixTree -> [Int]
getIndices (Leaf index)  = [index]
getIndices (Node myList) = concatMap (getIndices . snd)  myList

partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition a b = (match, a', b')
  where
    match       = map fst (takeWhile (uncurry (==)) (zip a b))
    matchLength = length match
    a'          = drop matchLength a
    b'          = drop matchLength b



findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' s (Node subTrees) = concat [ if (null s') then (getIndices b) else (findSubstrings' s' b) | (a, b) <- subTrees, let (match, s', a') = partition s a, not (null match) || null s'  ]
findSubstrings' s (Leaf index) = [index]

------------------------------------------------------

insert :: (String, Int) -> SuffixTree -> SuffixTree
insert val@(s, n) (Node subTrees)
  | newSubtrees == subTrees = Node ((s, Leaf n): subTrees)
  | otherwise = Node newSubtrees
  where 
    newSubtrees = [if (not (null overlap) || null remainingInsert) then (if (overlap == s') then (s', insert (remainingInsert, n) t') else((overlap, Node [(remainingInsert, Leaf n), (remainingOther, t')] ))) else st'| st'@(s', t') <- subTrees, let (overlap, remainingInsert, remainingOther) = partition s s']


-- This function is given
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

------------------------------------------------------
-- Part IV

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring (Node myTrees) = 
    let
      myList = "" : [a ++ longestRepeatedSubstring b | (a, b@(Node bs)) <- myTrees, length bs > 1] 
      maxLength = (maximum . map (length)) myList
      newList = filter (((==) maxLength) . length) myList
    in 
      head newList


------------------------------------------------------
-- Example strings and suffix trees...

s1 :: String
s1 
  = "banana"

s2 :: String
s2 
  = "mississippi"

t1 :: SuffixTree
t1 
  = Node [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 
  = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]


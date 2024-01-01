type BinHeap a = [BinTree a]

data BinTree a = Node a Int (BinHeap a)
               deriving (Eq, Ord, Show)

--------------------------------------------------------------
-- PART I

value :: BinTree a -> a
value (Node a b c)
  = a

rank :: BinTree a -> Int
rank (Node a b c)
  = b

children :: BinTree a -> [BinTree a]
children (Node a b c)
  = c

combineTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
combineTrees tree1 tree2
  | value1 < value2 = Node value1 newRank (tree2: children tree1)
  | otherwise = Node value2 newRank (tree1: children tree2)
    where
      value1 = value tree1
      value2 = value tree2
      newRank = rank tree1 + 1

--------------------------------------------------------------
-- PART II

extractMin :: Ord a => BinHeap a -> a
extractMin
  = minimum . map (value) 

mergeHeaps :: Ord d => BinHeap d -> BinHeap d -> BinHeap d
mergeHeaps [] a = a
mergeHeaps a [] = a
mergeHeaps h1@(a: as) h2@(b: bs) 
  | rankA > rankB = b : mergeHeaps bs h1
  | rankB > rankA = a : mergeHeaps as h2
  | otherwise = mergeHeaps  [(combineTrees a b)] (mergeHeaps as bs)
    where
      rankB = rank b
      rankA = rank a

insert :: Ord a => a -> BinHeap a -> BinHeap a
insert insertValue 
  = mergeHeaps [Node insertValue 0 []] 

deleteMin :: Ord a => BinHeap a -> BinHeap a
deleteMin myHeap = mergeHeaps a' b
  where
    (a, b) = removeMin myHeap
    a' = reverse (children a)

remove :: Eq a => a -> BinHeap a -> BinHeap a
remove val myHeap = filter (\x -> value x /= val) myHeap

removeMin :: Ord a => BinHeap a -> (BinTree a, BinHeap a)
removeMin heap
  = (treePreDelete, newHeap)
  where 
    min = extractMin heap
    (treePreDelete: otherMinimaTrees) = filter (\x -> value x == min) heap
    newHeap = remove min heap

binSort :: Ord a => [a] -> [a]
binSort inputList
  = sorted
  where
    heap = foldl (flip $ insert) [] inputList
    sorted = recursiveHeapRemover heap 

    recursiveHeapRemover :: Ord a => BinHeap a -> [a]
    recursiveHeapRemover [] = []
    recursiveHeapRemover myHeap = a: recursiveHeapRemover a'
      where 
        a = extractMin myHeap
        a' = deleteMin myHeap

--------------------------------------------------------------
-- PART III

toBinary :: BinHeap a -> [Int]
toBinary
  = undefined

binarySum :: [Int] -> [Int] -> [Int]
binarySum a b
  = sumZero (zip a b) 
  where 
    sumZero ::  [(Int, Int)] -> [Int]
    sumZero [] = []
    sumZero ((1, 1): xs) = 0 : sumOne xs
    sumZero ((0, 0): xs) = 0: sumZero xs
    sumZero (x: xs) = 1: sumZero xs
    sumOne :: [(Int, Int)] -> [Int]
    sumOne [] = []
    sumOne ((1, 1): xs) = 1 : sumOne xs
    sumOne ((0, 0): xs) = 1: sumZero xs
    sumOne (x: xs) = 0: sumOne xs
 

------------------------------------------------------
-- Some sample trees...

t1, t2, t3, t4, t5, t6, t7, t8 :: BinTree Int
-- Note: t7 is the result of merging t5 and t6

-- t1 to t4 appear in Figure 1...
t1 = Node 4 0 []
t2 = Node 1 1 [Node 5 0 []]
t3 = Node 2 2 [Node 8 1 [Node 9 0 []], 
               Node 7 0 []]
t4 = Node 2 3 [Node 3 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- t5 and t6 are on the left of Figure 2; t7 is on the
-- right
t5 = Node 4 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []]
t6 = Node 2 2 [Node 8 1 [Node 9 0 []], Node 7 0 []]
t7 = Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []], Node 10 0 []],
               Node 8 1 [Node 9 0 []], 
               Node 7 0 []]

-- An additional tree...
t8 = Node 12 1 [Node 16 0 []]

------------------------------------------------------
-- Some sample heaps...

h1, h2, h3, h4, h5, h6, h7 :: BinHeap Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 2 [Node 12 1 [Node 16 0 []],
                Node 5 0 []],
      Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 8 1 [Node 9 0 []],
                Node 7 0 []]]

-- h3 is shown in Figure 3...
h3 = [t1, t2, t4]

-- Two additional heaps, used below. They are shown
-- in Figure 4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 is the result of merging h4 and h5, shown in Figure 4(b)...
h6 = [Node 4 0 [],
      Node 1 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 12 1 [Node 16 0 []],
                Node 5 0 []]]

-- h7 is shown in Figure 5...
h7 = [Node 4 3 [Node 4 2 [Node 12 1 [Node 16 0 []],
                          Node 5 0 []],
                Node 6 1 [Node 8 0 []],
                Node 10 0 []]]
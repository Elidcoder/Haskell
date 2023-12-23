module Tries where

import Data.List hiding (insert)
import Data.Bits

import Types
import HashFunctions
import Examples

--------------------------------------------------------------------
-- Part I

-- Use this if you're counting the number of 1s in every
-- four-bit block...
bitTable :: [Int]
bitTable
  = [0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4]

countOnes :: Int -> Int
countOnes 0     = 0
countOnes input = (bitTable !! remainder) + countOnes quotient
  where 
    quotient  = input `div` 16
    remainder = input `mod` 16

countOnesFrom :: Int -> Int -> Int
countOnesFrom startPoint input
  = popCount (input .&. (bit startPoint - 1)) 

getIndex :: Int -> Int -> Int -> Int
getIndex enteredValue block blockSize = shiftR rightOfBlockKept noUnusedLSB
  where 
    noUnusedLSB      = blockSize * block
    rightOfBlockKept = (enteredValue .&. (bit (noUnusedLSB + blockSize ) - 1))

-- Pre: the index is less than the length of the list
replace :: Int -> [a] -> a -> [a]
replace 0 (x: xs) value = value: xs
replace n (x: xs) value = x: replace (n-1) xs value

-- Pre: the index is less than or equal to the length of the list
insertAt :: Int -> a -> [a] -> [a]
insertAt 0 value oldList = value: oldList
insertAt n value (x: xs) = x: insertAt (n-1) value xs

--------------------------------------------------------------------
-- Part II

sumTrie :: (Int -> Int) -> ([Int] -> Int) -> Trie -> Int
sumTrie _ f2Leaf (Leaf intList)
  = f2Leaf intList
sumTrie f1Term f2Leaf (Node _ subNodeList)
  = sum $ map undo subNodeList
  where 
    undo :: SubNode -> Int
    undo (Term    value) = f1Term value
    undo (SubTrie value) = sumTrie f1Term f2Leaf value



--
-- If you get the sumTrie function above working you can uncomment
-- these three functions; they may be useful in testing.
--
trieSize :: Trie -> Int
trieSize t
  = sumTrie (const 1) length t

binCount :: Trie -> Int
binCount t
  = sumTrie (const 1) (const 1) t

meanBinSize :: Trie -> Double
meanBinSize t
  = fromIntegral (trieSize t) / fromIntegral (binCount t)

member :: Int -> Hash -> Trie -> Int -> Bool
member value valueHash trieToCheck blockSize
  = checkLevel 0 trieToCheck 

  where
    --mask = bit (blockSize) - 1
    checkLevel :: Int -> Trie -> Bool
    checkLevel level (Leaf leafVals) = value `elem` leafVals
    checkLevel level (Node bitVector subNodes)
      | testBit bitVector i = 
        let 
          n       = countOnesFrom i bitVector
          subNode = subNodes !! n
        in
          evalTorST subNode
      | otherwise           = False 

        where 
          --modifiedMask = shiftL mask level
          i = getIndex valueHash level blockSize--valueHash .&. modifiedMask
          evalTorST :: SubNode -> Bool
          evalTorST (Term k)       = (k == value)
          evalTorST (SubTrie trie) = checkLevel (level + 1) trie


--------------------------------------------------------------------
-- Part III

insert :: HashFun -> Int -> Int -> Int -> Trie -> Trie
insert
  = undefined

buildTrie :: HashFun -> Int -> Int -> [Int] -> Trie
buildTrie
  = undefined
module Solver where

import Data.List
import Data.Char

import Types
import WordData
import Clues
import Examples

------------------------------------------------------
-- Part I

punctuation :: String
punctuation 
  = "';.,-!?"

cleanUp :: String -> String
cleanUp = filter (not . ( `elem` punctuation))


split2 :: [a] -> [([a], [a])]
split2 []     = []
split2 (x:[]) = []
split2 list = map (`splitAt` list) [1..(length list - 1)]

split3 :: [a] -> [([a], [a], [a])]
split3 list
  = split2WithEmpty ++ split2split2L
  where 
    split2List = split2 list
    split2WithEmpty = map (\x -> (fst x, [], snd x)) split2List
    split2split2L = concatMap (
      \x -> 
        let 
          (first, third)    = x
        in 
          [(first', second', third) | (first', second')  <- split2 first]
      ) split2List 

uninsert :: [a] -> [([a], [a])]
uninsert list =  [(x2, x1 ++ x3) | (x1, x2, x3) <- split3 list, not (null x2)]

split2M :: [a] -> [([a], [a])]
split2M xs
  = sxs ++ [(y, x) | (x, y) <- sxs] 
  where
    sxs = split2 xs

split3M :: [a] -> [([a], [a], [a])]
split3M xs
  = sxs ++ [(z, y, x) | (x, y, z) <- sxs]
  where
    sxs = split3 xs
------------------------------------------------------
-- Part II

matches :: String -> ParseTree -> Bool
matches
  = undefined

evaluate :: Parse -> Int -> [String]
evaluate 
  = undefined

------------------------------------------------------
-- Part III

-- Given...
parseWordplay :: [String] -> [ParseTree]
parseWordplay ws
  = concat [parseSynonym ws,
            parseAnagram ws,
            parseReversal ws,
            parseInsertion ws,
            parseCharade ws]
    
parseSynonym :: [String] -> [ParseTree]
parseSynonym 
  = const []

parseAnagram :: [String] -> [ParseTree]
parseAnagram
  = const []

parseReversal :: [String] -> [ParseTree]
parseReversal
  = const []

parseInsertion :: [String] -> [ParseTree]
parseInsertion
  = const []

parseCharade :: [String] -> [ParseTree]
parseCharade 
  = const []

-- Given...
parseClue :: Clue -> [Parse]
parseClue clue@(s, n)
  = parseClueText (words (cleanUp s))

parseClueText :: [String] -> [Parse]
parseClueText
  = undefined

solve :: Clue -> [Solution]
solve 
  = undefined


------------------------------------------------------
-- Some additional test functions

-- Returns the solution(s) to the first k clues.
-- The nub removes duplicate solutions arising from the
-- charade parsing rule.
solveAll :: Int -> [[String]]
solveAll k
  = map (nub . map getSol . solve . (clues !!)) [0..k-1]

getSol :: Solution -> String
getSol (_, _, sol) = sol

showAll
  = mapM_ (showSolutions . solve . (clues !!)) [0..23]


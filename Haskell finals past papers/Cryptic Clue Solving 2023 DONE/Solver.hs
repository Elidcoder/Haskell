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
cleanUp s = map (toLower) (filter (not . ( `elem` punctuation)) s)


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
matches s (Synonym s') = s `elem` (synonyms s')
matches s (Anagram _ s') = (sort s) == (sort s')
matches s (Reversal _ t) = matches (reverse s) t
matches s (Insertion _ t1 t2) = 
  not (null (filter (\(a,b) -> (matches a t1) && (matches b t2)) (uninsert s)))
matches s (Charade _ t1 t2) 
  = not (null (filter (\(a,b) -> (matches a t1) && (matches b t2)) (split2 s)))
matches s (HiddenWord _ s') 
  | (a: []) <- input = s `elem` (map (\(a,b,c) -> b) (split3 a))
  | otherwise =  s `elem` final
  where
    input       = words s'
    removeFL    = init . tail
    firstWord   = (head input)
    firstUsable = (tails . removeFL) firstWord
    lastWord    = (last input)
    lastUsable  = map reverse ((tails . removeFL . reverse) lastWord)
  
    usable      = (last firstWord) : concat(removeFL input) ++ [head lastWord]
    final       = [f ++ usable ++ l | f <- firstUsable, l <- lastUsable]

evaluate :: Parse -> Int -> [String]
evaluate (definition, link, parseTree) solLength
  = filter (`matches` parseTree) (filter ((solLength ==) . length) (synonyms (unwords definition)))

------------------------------------------------------
-- Part III

-- Given...
parseWordplay :: [String] -> [ParseTree]
parseWordplay ws
  = concat [parseSynonym ws,
            parseAnagram ws,
            parseReversal ws,
            parseInsertion ws,
            parseCharade ws,
            parseHiddenWords ws]
    
parseSynonym :: [String] -> [ParseTree]
parseSynonym input
  | null inputSynonyms = []
  | otherwise = [Synonym parseInput]
    where
      parseInput    = unwords input
      inputSynonyms = synonyms parseInput

parseAnagram :: [String] -> [ParseTree]
parseAnagram inputs = [Anagram a (concat b) | (a, b) <- (split2M inputs), ((unwords a) `elem` anagramIndicators) ]

parseReversal :: [String] -> [ParseTree]
parseReversal inputs = [Reversal a c | (a, b) <- (split2M inputs), ((unwords a) `elem` reversalIndicators), c <- parseWordplay b]

parseInsertion :: [String] -> [ParseTree]
parseInsertion input = finals
  where
    properTriplets = envelopeOrInsert (split3 input)
    finals = [Insertion b a' c'| (a, b, c) <- properTriplets, a' <- a, c' <- c]
    
    envelopeOrInsert [] = []
    envelopeOrInsert ((a, b, c):  xs)
      | b' `elem` insertionIndicators = (a', b, c') : rest
      | b' `elem` envelopeIndicators  = (c', b, a') : rest
      | otherwise = rest
        where
          rest = envelopeOrInsert xs
          b' = unwords b
          a' = parseWordplay a
          c' = parseWordplay c
    
parseCharade :: [String] -> [ParseTree]
parseCharade input = finals
  where
    properTriplets = envelopeOrInsert (split3 input)
    finals = [Charade b a' c'| (a, b, c) <- properTriplets, a' <- a, c' <- c]
    
    envelopeOrInsert [] = []
    envelopeOrInsert ((a, b, c):  xs)
      | b' `elem` beforeIndicators = (a', b, c') : rest
      | b' `elem` afterIndicators  = (c', b, a') : rest
      | otherwise = rest
        where
          rest = envelopeOrInsert xs
          b' = unwords b
          a' = parseWordplay a
          c' = parseWordplay c

parseHiddenWords :: [String] -> [ParseTree]
parseHiddenWords inputs = [HiddenWord a (unwords b) | (a, b) <- split2 inputs, (unwords a) `elem` hiddenWordIndicators]
      
      



-- Given...
parseClue :: Clue -> [Parse]
parseClue clue@(s, n)
  = parseClueText (words (cleanUp s))

parseClueText :: [String] -> [Parse]
parseClueText input = 
  [(def, oLink, wordplay') | (def, oLink, wordplay) <- split3M input, ((`elem` linkWords) . unwords) oLink, (not . null . synonyms . unwords) def, wordplay' <- parseWordplay wordplay ]

solve :: Clue -> [Solution]
solve c@(_, len)
  = [(c, c'',s) | c'' <- c', s <- evaluate c'' len]
  where
    c' = parseClue c
     



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


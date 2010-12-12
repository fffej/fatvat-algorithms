module WordLadders where

import qualified Data.Set as S

import Data.Char
import Data.List (sortBy,(\\),tails,inits)
import Data.Ord (comparing,Ord)

type Word = String

type WordSet = S.Set Word                                   
data Node = Node Word [Node]

data DistanceMetric = DistanceMetric (Word -> Word -> Int) (Word -> WordSet)
                      
validChars :: [Char]
validChars = "abcdefghijklmnopqrstuvwxyz"

difference :: Word -> Word -> Int                     
difference x y 
  | length x /= length y = 999999
  | otherwise = sum $ zipWith (\c1 c2 -> if c1 == c2 then 0 else 1) x y
                
transposeChar :: Word -> [Word] 
transposeChar [] = []
transposeChar (x:xs) = map (:xs) (validChars \\ [x])
                
deleteChar :: Word -> [Word]                       
deleteChar [] = []
deleteChar (x:xs) = [xs]

insertChar :: Word -> [Word]
insertChar [] = []
insertChar (x:xs) = map (\y -> y:x:xs) validChars

differenceEdit :: Word -> WordSet
differenceEdit x = edit' x [transposeChar]
    
simple :: DistanceMetric
simple = DistanceMetric difference differenceEdit

edits :: DistanceMetric
edits = DistanceMetric editDistance editDistanceEdits

editDistanceEdits :: Word -> WordSet
editDistanceEdits x = edit' x [insertChar,transposeChar,deleteChar]

edit' :: Word -> [Word -> [Word]] -> WordSet
edit' w fns = S.fromList $ concat $ zipWith (\x y -> map (\z -> x ++ z) (concatMap (\x -> x y) fns)) (inits w) (tails w)

-- Grabbed from http://www.haskell.org/haskellwiki/Edit_distance
editDistance :: Word -> Word -> Int
editDistance a b 
    = last (if lab == 0 then mainDiag
	    else if lab > 0 then lowers !! (lab - 1)
		 else{- < 0 -}   uppers !! (-1 - lab))
    where 
          mainDiag = oneDiag a b (head uppers) (-1 : head lowers)
	  uppers = eachDiag a b (mainDiag : uppers) -- upper diagonals
	  lowers = eachDiag b a (mainDiag : lowers) -- lower diagonals
	  eachDiag a [] diags = []
	  eachDiag a (bch:bs) (lastDiag:diags) = oneDiag a bs nextDiag lastDiag : eachDiag a bs diags
	      where nextDiag = head (tail diags)
	  oneDiag a b diagAbove diagBelow = thisdiag
	      where doDiag [] b nw n w = []
		    doDiag a [] nw n w = []
		    doDiag (ach:as) (bch:bs) nw n w = me : doDiag as bs me (tail n) (tail w)
			where me = if ach == bch then nw else 1 + min3 (head w) nw (head n)
		    firstelt = 1 + head diagBelow
		    thisdiag = firstelt : doDiag a b firstelt diagAbove (tail diagBelow)
	  lab = length a - length b
          min3 x y z = if x < y then x else min y z

neighbour :: DistanceMetric -> Word -> Word -> Bool
neighbour (DistanceMetric d _) x y = d x y == 1

makeLadder :: DistanceMetric -> Int -> Word -> Word -> IO [Word]
makeLadder d maxDepth start end = do    
      dict <- createDictionary
      if S.member start dict && S.member end dict
        then return $ search d (buildGraph d dict end) maxDepth start
        else return []
             
wordListPath :: Word
wordListPath = "/usr/share/dict/british-english"

buildGraph :: DistanceMetric -> WordSet -> Word -> Node 
buildGraph d@(DistanceMetric dist edits) wordset top = Node top (map (buildGraph d smaller) neighbours)
  where
    possibleNeighbours = edits top
    neighbours = S.toList (smaller `S.intersection` possibleNeighbours)
    smaller = S.delete top wordset 
    
search :: DistanceMetric -> Node -> Int -> Word -> [Word]
search (DistanceMetric dist _) graph maxDepth goal = search' graph []
  where 
    search' (Node end children) path 
      | end == goal    = end : path 
      | length path >= maxDepth = [] -- too deep
      | dist end goal >= maxDepth - length path = [] -- too much difference
      | otherwise = first
        where
          -- Find the best node to search by comparing it against the goal
          costForNextChild :: [(Int,Node)]
          costForNextChild = zip (map (\(Node x _) -> dist x goal) children) children
          bestFirst = map snd $ sortBy (comparing fst) costForNextChild
      
          -- Best first search
          childRoutes = filter (not . null) $ map (\child -> search' child (end : path)) bestFirst
      
          first | null childRoutes = []
                | otherwise        = head childRoutes                                    
                                                  
    
createDictionary :: IO WordSet    
createDictionary = do
  file <- readFile wordListPath 
  return $ S.fromList $ filter (all isAlpha) (map (map toLower) $ words file)



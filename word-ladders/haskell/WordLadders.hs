module WordLadders where

import qualified Data.Set as S

import Data.Char
import Data.List (minimumBy,sortBy,(\\),tails,inits)
import Data.Ord (comparing,compare,Ord)

type Word = String

type WordSet = S.Set Word                                   
data Node = Node Word [Node]
type DistanceMetric = Word -> Word -> Int
type Edits = Word -> WordSet

{- Cost Functions -}                      
                      
validChars :: [Char]
validChars = "abcdefghijklmnopqrstuvwxyz"

difference :: Word -> Word -> Int                     
difference x y 
  | length x /= length y = 999999
  | otherwise = sum $ zipWith (\c1 c2 -> if c1 == c2 then 0 else 1) x y
                
                
differentEdit :: Word -> WordSet
differentEdit x = S.fromList $ concat $ zipWith (\x y -> map (\z -> x ++ z) (transposeChar y)) (inits x) (tails x)
  where
    transposeChar [] = []
    transposeChar (x:xs) = map (\y -> y:xs) (validChars \\ [x])

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
neighbour dist x y = dist x y == 1

makeLadder :: DistanceMetric -> Edits -> Int-> Int -> Word -> Word -> IO [Word]
makeLadder d e maxDepth maxVariation start end = do    
      dict <- createDictionary
      if S.member start dict && S.member end dict
        then return $ search d (buildGraph d e dict end) maxDepth maxVariation start
        else return []
             
wordListPath :: Word
wordListPath = "/usr/share/dict/british-english"

buildGraph :: DistanceMetric -> Edits -> WordSet -> Word -> Node 
buildGraph distanceMetric edits wordset top = Node top (map (buildGraph distanceMetric edits smaller) neighbours)
  where
    possibleNeighbours = edits top
    neighbours = S.toList (smaller `S.intersection` possibleNeighbours)
    smaller = S.delete top wordset 
    
search :: DistanceMetric -> Node -> Int -> Int -> Word -> [Word]
search dist graph maxDepth maxVariation goal = search' graph []
  where 
    search' (Node end children) path 
      | end == goal    = end : path 
      | length path >= maxDepth = [] -- too deep
      | dist end goal >= maxDepth - length path = [] -- too much difference
      | dist end goal >= maxVariation = [] -- too far off in the wrong direction
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
      
          quickest | null childRoutes = []
                   | otherwise = minimumBy (comparing length) childRoutes
                                            
    
createDictionary :: IO WordSet    
createDictionary = do
  file <- readFile wordListPath 
  return $ S.fromList $ filter (all isAlpha) (map (map toLower) $ words file)



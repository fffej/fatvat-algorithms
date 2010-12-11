module WordLadders where

import qualified Data.Set as S
import Data.Char
import Data.List (minimumBy)
import Data.Ord (comparing)

-- |Each node carries details of the word
data Node = Node String [Node] deriving Show

type WordSet = S.Set String
 
data Dictionary = Dictionary WordSet

type DistanceMetric = String -> String -> Int
                                   
difference :: String -> String -> Int                     
difference [] [] = 0
difference (x:xs) (y:ys) | x == y = difference xs ys
                         | otherwise = 1 + difference xs ys
difference _ _ = error "Two strings must be the same length"

-- Grabbed from http://www.haskell.org/haskellwiki/Edit_distance
editDistance :: Eq a => [a] -> [a] -> Int
editDistance a b 
    = last (if lab == 0 then mainDiag
	    else if lab > 0 then lowers !! (lab - 1)
		 else{- < 0 -}   uppers !! (-1 - lab))
    where mainDiag = oneDiag a b (head uppers) (-1 : head lowers)
	  uppers = eachDiag a b (mainDiag : uppers) -- upper diagonals
	  lowers = eachDiag b a (mainDiag : lowers) -- lower diagonals
	  eachDiag a [] diags = []
	  eachDiag a (bch:bs) (lastDiag:diags) = oneDiag a bs nextDiag lastDiag : eachDiag a bs diags
	      where nextDiag = head (tail diags)
	  oneDiag a b diagAbove diagBelow = thisdiag
	      where doDiag [] b nw n w = []
		    doDiag a [] nw n w = []
		    doDiag (ach:as) (bch:bs) nw n w = me : (doDiag as bs me (tail n) (tail w))
			where me = if ach == bch then nw else 1 + min3 (head w) nw (head n)
		    firstelt = 1 + head diagBelow
		    thisdiag = firstelt : doDiag a b firstelt diagAbove (tail diagBelow)
	  lab = length a - length b
          min3 x y z = if x < y then x else min y z

-- Two strings are a neighbour if they differ by only a single character
neighbour :: DistanceMetric -> String -> String -> Bool
neighbour dist x y = dist x y == 1

makeLadder :: DistanceMetric -> Int-> String -> String -> IO [String]
makeLadder d maxDepth start end = do    
      dict <- createDictionary
      if (S.member start dict && S.member end dict)
        then return $ search d (buildGraph d dict start) maxDepth end
        else return []
             
wordListPath :: String
wordListPath = "/usr/share/dict/british-english"

buildGraph :: DistanceMetric -> WordSet -> String -> Node 
buildGraph distanceMetric wordset top = Node top (map (buildGraph distanceMetric smaller) neighbours)
  where
    neighbours = S.toList (S.filter (neighbour distanceMetric top) smaller)
    smaller = S.delete top wordset 
    
drawGraph :: Node -> [String]
drawGraph (Node a children) = (map (\(Node child _) -> a ++ " -> " ++ child) children) ++  
                              (concatMap drawGraph children)
search :: DistanceMetric -> Node -> Int -> String -> [String]
search distanceMetric graph maxDepth goal = search' distanceMetric graph maxDepth goal []

search' :: DistanceMetric -> Node -> Int -> String -> [String] -> [String]
search' dist (Node end children) maxDepth goal path 
  | end == goal    = end : path 
  | null children  = [] 
  | length path >= maxDepth = [] -- too deep
  | dist end goal >= maxDepth - length path = [] -- too much difference
  | otherwise = first
    where
      childRoutes = filter (not . null) $ 
                    map (\child -> search' dist child maxDepth goal (end : path)) children
      first | null childRoutes = []
            | otherwise        = head childRoutes                                    
      quickest | null childRoutes = []
               | otherwise = minimumBy (comparing length) childRoutes
                                            
    
createDictionary :: IO WordSet    
createDictionary = do
  file <- readFile wordListPath 
  return $ S.fromList $ filter (\x -> all isAlpha x) (map (map toLower) $ words file)



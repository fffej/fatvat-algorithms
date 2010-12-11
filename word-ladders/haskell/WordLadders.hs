module WordLadders where

import qualified Data.Set as S
import Data.Char
import Data.List (minimumBy)
import Data.Ord (comparing)

-- |Each node carries details of the word
data Node = Node String [Node] deriving Show

type WordSet = S.Set String
 
data Dictionary = Dictionary WordSet

data Comparison = Comparison {
  isNeighbour :: String -> String -> Bool,
  distance :: String -> String -> Int
}

makeLadder :: Int-> String -> String -> IO [String]
makeLadder maxDepth start end 
  | length start /= length end = error "Only two strings of equal length are currently supported."
  | otherwise = do    
      dict <- createDictionary (length start)
      if (S.member start dict && S.member end dict)
        then return $ search (buildGraph dict start) maxDepth end
        else return []
             
wordListPath :: String
wordListPath = "/usr/share/dict/british-english"

buildGraph :: WordSet -> String -> Node 
buildGraph wordset top = Node top (map (buildGraph smaller) neighbours)
  where
    neighbours = S.toList (S.filter (neighbour top) smaller)
    smaller = S.delete top wordset 
    
drawGraph :: Node -> [String]
drawGraph (Node a children) = (map (\(Node child _) -> a ++ " -> " ++ child) children) ++  
                              (concatMap drawGraph children)
search :: Node -> Int -> String -> [String]
search graph maxDepth goal = search' graph maxDepth goal []

search' :: Node -> Int -> String -> [String] -> [String]
search' (Node end children) maxDepth goal path 
  | end == goal    = end : path 
  | null children  = [] 
  | length path >= maxDepth = [] -- too deep
  | difference end goal >= maxDepth - length path = [] -- too much difference
  | otherwise = quickest
    where
      childRoutes = filter (not . null) $ 
                    map (\child -> search' child maxDepth goal (end : path)) children
      first | null childRoutes = []
            | otherwise        = head childRoutes                                    
      quickest | null childRoutes = []
               | otherwise = minimumBy (comparing length) childRoutes
                                            
-- Two strings are a neighbour if they differ by only a single character
neighbour :: String -> String -> Bool
neighbour x y = difference x y == 1
                                     
difference :: String -> String -> Int                     
difference [] [] = 0
difference (x:xs) (y:ys) | x == y = difference xs ys
                         | otherwise = 1 + difference xs ys
difference _ _ = error "Two strings must be the same length"
    
createDictionary :: Int -> IO WordSet    
createDictionary n = do
  file <- readFile wordListPath 
  return $ S.fromList $ filter (\x -> length x == n && all isAlpha x) (map (map toLower) $ words file)



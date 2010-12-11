module WordLadders where

import qualified Data.Set as S
import Data.Char
import Control.Monad
import Data.List
import Data.Ord (comparing)

-- |Each node carries details of the word
data Node = Node String [Node] deriving Show

type WordSet = S.Set String
 
data Dictionary = Dictionary WordSet

wordListPath :: String
wordListPath = "/usr/share/dict/british-english"

makeLadder :: Dictionary -> String -> String -> [String]
makeLadder = undefined

buildGraph :: WordSet -> String -> Node 
buildGraph wordset head = Node head (map (buildGraph smaller) neighbours)
  where
    neighbours = S.toList (S.filter (neighbour head) smaller)
    smaller = S.delete head wordset 
    
-- TODO restrict to a maximum depth

search :: Node -> Int -> String -> [String]
search graph maxDepth goal = search' graph maxDepth goal []

search' :: Node -> Int -> String -> [String] -> [String]
search' (Node end children) maxDepth goal path 
  | end == goal    = end : path -- finished
  | null children  = [] -- no where left to goal
  | length path >= maxDepth = [] -- too deep
  | (difference end goal) >= maxDepth - length path = [] -- too much difference
  | otherwise = quickest
    where
      -- search all the children
      childRoutes = filter (not . null) $ map (\child -> search' child maxDepth goal (end : path)) children
      quickest | null childRoutes = []
               | otherwise        = minimumBy (comparing length) (filter (not . null) childRoutes)
                                        


    
-- Two strings are a neighbour if they differ by only a single character
neighbour :: String -> String -> Bool
neighbour x y = difference x y == 1
                     
difference :: String -> String -> Int                     
difference [] [] = 0
difference (x:xs) (y:ys) | x == y = difference xs ys
                         | otherwise = 1 + difference xs ys
    
createDictionary :: Int -> IO WordSet    
createDictionary n = do
  file <- readFile wordListPath 
  return $ S.fromList $ filter (\x -> length x == n && all isAlpha x) (map (map toLower) $ words file)



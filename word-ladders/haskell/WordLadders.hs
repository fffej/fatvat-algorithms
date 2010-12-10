module WordLadders where

import qualified Data.Set as S
import Data.Char
import Control.Monad
import Data.List
import Data.Ord (comparing)

-- |Each node carries details of the word
data Node a = Node a [Node a] deriving Show

type WordSet = S.Set String
 
data Dictionary = Dictionary WordSet

wordListPath :: String
wordListPath = "/usr/share/dict/british-english"

makeLadder :: Dictionary -> String -> String -> [String]
makeLadder = undefined

buildGraph :: WordSet -> String -> Node String
buildGraph wordset head = Node head (map (buildGraph smaller) neighbours)
  where
    neighbours = S.toList (S.filter (neighbour head) smaller)
    smaller = S.delete head wordset 
    
-- TODO restrict to a maximum depth

search :: Eq a => Node a -> a -> [a]
search graph goal = search' graph goal []

search' (Node end children) goal path | end == goal    = end : path -- finished
                                      | null children  = [] -- no where left to goal
                                      | otherwise = quickest
                                        where
                                          -- search all the children
                                          childRoutes = map (\x -> search' x goal (end : path)) children
                                          quickest = minimumBy (comparing length) childRoutes
                                        


    
-- Two strings are a neighbour if they differ by only a single character
neighbour :: String -> String -> Bool
neighbour x y = neighbour' x y 0 
  where
    neighbour' _ _ 2  = False
    neighbour' [] [] c = c == 1
    neighbour' (x:xs) (y:ys) c = neighbour' xs ys (c + d)
      where
        d | x == y = 0
          | x /= y = 1
    
createDictionary :: Int -> IO WordSet    
createDictionary n = do
  file <- readFile wordListPath 
  return $ S.fromList $ filter (\x -> length x == n && all isAlpha x) (map (map toLower) $ words file)



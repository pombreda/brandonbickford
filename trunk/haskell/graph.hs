import Control.Monad.State
import qualified Data.Set as Set
import Data.Maybe
import qualified Data.List as List

-- slightly different impl of the stateful searchall found at the end of YAHT
-- this would be more efficient with a Map to track edge relations and a Set to track visitor membership

search :: [(Int, Int)] -> Int -> Int -> [[(Int, Int)]] 
search edges start end =  concatMap (\ed -> (evalState (search' ed) [])) edges
   where search' :: (Int, Int) -> State [[(Int, Int)]] [[(Int, Int)]]
         search' (edge@(s, e)) 
            | not (s == start) = return []
            | s == start && e == end = return [[edge]]
            | otherwise = do
                  mapM (\ed -> search'' ed e []) edges 
                  st <- get
                  return $ map (edge:) st 

         search'':: (Int, Int) -> Int -> [(Int, Int)] -> State [[(Int, Int)]] ()
         search'' (edge@(s, e)) start' visited 
            | not (s == start') = return ()
            | edge `elem` visited = return ()
            | s == start' && e == end = do
                st <- get
                put $ (reverse (edge:visited)):st
                return () 
            | s == start' = do
                let visited' = edge:visited
                mapM (\edge -> search'' edge e visited') edges 
                return ()
        
main = do
   print $ search [(1, 1)] 1 1 
   print $ search [] 1 1
   print $ search [(1, 2), (2,5), (5,6), (2,4), (4,5)] 1 6
   print $ search [(1, 2), (1,1), (2,5), (5,6), (2,4), (4,5)] 1 6


            
               
         
   

import AocUtils
import Control.Exception (Exception, throw)
import Text.Read (readMaybe)
import Control.Arrow ((&&&))
import qualified Data.Map as M
import Data.Maybe (fromMaybe, fromJust)
import Control.Monad.State
    ( evalState, MonadState(put, get), State )
import Control.Monad (mapM)
import Data.List (find)

main :: IO ()
main = do
       s <- getInput 3
       print $ solution $ parse s

parse :: String -> Int
parse = read :: (String -> Int)

ringNum :: Int -> Int
ringNum = (flip div) 2 . ceiling . sqrt . fromIntegral

overflowMove :: Int -> Int -> (Int, Int)
overflowMove rn d 
                 | d  <= y = (0, d)
                 | d  <= 2 * y = (-(d-y), y)
                 | d  <= 3 * y = (-y, y - (d - 2*y))
                 | d  <= 4 * y = (-(y - (d - 3*y )), 0)
		 where y = 2*rn

coords :: Int -> (Int, Int)
coords d = let rn = ringNum d 
	       (x, y) = (rn, -rn)
               ovf = d - (2*rn - 1)^2 
               (dx, dy) = overflowMove rn ovf
                 in 
	         (x + dx, y + dy)

type LookupTable = M.Map (Int, Int) Int

addTup :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addTup (x, y) (a, b)= (x + a, y + b)


perim :: [(Int, Int)]
perim = [(1, 0),
        (1, 1),
        (0, 1),
        (-1, 1),
        (-1, 0),
        (-1,-1),
        (0, -1),
        (1, -1)]

compute :: Int -> State LookupTable Int
compute d = do 
        let c = coords d
        let toCheck = map (addTup c) perim
        m <- get
	let s = sum $ map (fromMaybe 0 . (flip M.lookup) m) toCheck 
	put $ M.insert c s m
        return s

stressTest :: [Int]
stressTest = 1 : evalState (sequence (map compute [2..])) (M.insert (0, 0) 1 M.empty)

solution :: Int -> Int
solution x = fromJust $ find (>x) stressTest

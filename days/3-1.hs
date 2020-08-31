import AocUtils
import Control.Exception (Exception, throw)
import Text.Read (readMaybe)
import Control.Arrow ((&&&))

main :: IO ()
main = do
       s <- getInput 3
       print $ solution $ parse s

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

coords d = let rn = ringNum d 
	       (x, y) = (rn, -rn)
               ovf = d - (2*rn - 1)^2 
               (dx, dy) = overflowMove rn ovf
                 in 
	         (x + dx, y + dy)
			 
solution z = let (x, y) = coords z in (abs x) + (abs y)


import AocUtils

main :: IO ()
main = do
       s <- getInput 2
       print $ head $ parse s

parse :: String -> [[Int]]
parse = map (map read . words) . lines 

maxAndMin :: [Int] -> (Int, Int)
maxAndMin (x:xs) = foldr f (x, x) xs 
	where f c (a, b) = (max a c, min b c)

solution = sum . map go 
                where go l = ma - mi
                          where (ma, mi) = maxAndMin l


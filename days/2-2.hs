import AocUtils
import Data.Maybe (fromJust)
import Control.Applicative ((<|>))
import Data.List (sort, find)

main :: IO ()
main = do
       s <- getInput 2
       print $ solution $ parse s

parse :: String -> [[Int]]
parse = map (map read . words) . lines 

solution :: [[Int]] -> Int
solution = sum . map (fromJust . divisors . sort)

divisors :: [Int] -> Maybe Int 
divisors (x : xs) = fmap ((flip div) x) (find (\z -> (z `mod` x) == 0) xs) <|> (divisors xs)
divisors [] = Nothing

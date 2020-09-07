import AocUtils
import Control.Exception (Exception, throw)
import Text.Read (readMaybe)
import Control.Arrow ((&&&))
import Data.List (group, sort)

main :: IO ()
main = do
       s <- getInput 4
       print $ head $ parse s

parse :: String -> [[String]]
parse = map words . lines

valid :: [String] -> Bool 
valid = not . any ((>1) . length) . group . sort  

solution = length . filter valid

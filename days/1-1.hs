import AocUtils
import Control.Exception (Exception, throw)
import Text.Read (readMaybe)

main :: IO ()
main = do
       s <- getInput 1
       p <- parse s
       print $ solution p

data Error = ParseFailure
     deriving Show

instance Exception Error

parse :: String -> IO [Int]
parse = makeErr . sequence . map ((readMaybe :: String -> (Maybe Int)) . (:[]))
        where makeErr e = case e of 
                              Just x -> return x
                              Nothing -> throw ParseFailure

solution :: [Int] -> Int
solution l = fst $ foldl go (0, (last l)) l
          where go (cum, prev) elem = if prev == elem then (cum + elem, elem) else (cum, elem)


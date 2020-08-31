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

rotate :: Int -> [a] -> [a]
rotate n l = let (x, y) = splitAt n l in y ++ x 

dist :: [a] -> Int
dist l = div (length l) 2

solution :: [Int] -> Int
solution l = sum  $ map snd $ filter (\(x, y) -> x==y) $ zip l (rotate (dist l) l)

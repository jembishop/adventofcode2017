import AocUtils ( getInput )
import Data.List ( maximumBy )
import Debug.Trace ()
import Data.Ord ( Down(Down), comparing )
import qualified Data.Set as S

main :: IO ()
main = do
  s <- getInput 6
  print $ solution $ parse s

parse :: String -> [Int]
parse = map read . words

solution :: [Int] -> Int
solution l = findDup $ iterate redistribute l

rotate :: Int -> [a] -> [a]
rotate n l = take (length l) $ drop n (cycle l)

incFirstN :: Num a => Int -> [a] -> [a]
incFirstN n l = (map (+ 1) fh) ++ sh where (fh, sh) = splitAt n l

zeroAtN :: Num a => Int -> [a] -> ([a], a)
zeroAtN n l = (fh ++ 0 : shr, shf) where (fh, shf : shr) = splitAt n l

remainderAdd :: Int -> Int -> [Int] -> [Int]
remainderAdd i n l = (rotate mi  . incFirstN n . rotate i) l
  where
    mi = (length l) - i 

maxIdx :: [Int] -> Int
maxIdx = fst . maximumBy (comparing (\(x,y) -> (y, Down x))). zip [0 ..]

redistribute :: [Int] -> [Int]
redistribute l = remainderAdd (maxI +1) rem $ map (+ div) zeroed
  where
    (div, rem) = divMod max (length l)
    (zeroed, max) = zeroAtN maxI l
    maxI = maxIdx l

findDup :: Ord a => [a] -> Int
findDup l = go 0 l S.empty
    where go i (x:xs) s = if S.member x s then i else go (i+1) xs (S.insert x s)
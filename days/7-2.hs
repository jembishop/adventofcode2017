{-# LANGUAGE QuasiQuotes #-}

import AocUtils (getInput)
import Control.Monad (join)
import Data.Fix (Fix (..), unFix, unfoldFix)
import Data.List (find, maximumBy, minimumBy)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Debug.Trace
import Text.RawString.QQ (r)
import Text.Regex.TDFA (getAllTextMatches, (=~))

mainRegex :: [Char]
mainRegex = [r|([a-z]+) \(([0-9]+)\)(:? -> (.*))?|]

data TowerF a = TowerF String Int [a]
  deriving (Show)

instance Functor TowerF where
  fmap f (TowerF x y l) = TowerF x y (fmap f l)

type TowerMap = M.Map String (TowerF String)

type Tower = Fix TowerF

parseOne :: String -> TowerF String
parseOne s = TowerF (m !! 1) (read (m !! 2)) rest
  where
    m = (s =~ mainRegex :: [[String]]) !! 0
    rest = getAllTextMatches ((m !! 4) =~ "[a-z]+") :: [String]

parse :: String -> TowerMap
parse = M.fromList . map (\x -> let p = parseOne x; TowerF s _ _ = p in (s, p)) . lines

makeTowerFStack :: TowerMap -> [Tower]
makeTowerFStack tm = map (unfoldFix stringToTowerF) (M.keys tm)
  where
    stringToTowerF s = fromJust (M.lookup s tm)

towerLength :: Tower -> Int
towerLength = go 0
  where
    go :: Int -> Tower -> Int
    go ix t = case l of
      [] -> ix
      otherwise -> maximum $ map (go (ix + 1)) l
      where
        (TowerF s i l) = unFix t

towerWeight :: Tower -> Tower
towerWeight t = Fix (TowerF n newWeight c)
  where
    newWeight = (w + weight c)
    (TowerF n w c) = unFix t
    weight = sum . map (getWeight . towerWeight)

getWeight :: Tower -> Int
getWeight t = w
  where
    (TowerF n w c) = unFix t

baseTower :: [Tower] -> Tower
baseTower = maximumBy (comparing towerLength)

searchForUnbalanced :: Tower -> [(String, (Int, Int))]
searchForUnbalanced = help []
  where
    help l t = case (counts c) of
      [] -> []
      [(_, _)] -> l ++ rest
      [(a, (m, b)), (x, (w, y))] -> (if b > y then (w !! 0, (a, x)) else (m !! 0, (x, a))) : rest
      otherwise -> error "impossible"
      where
        rest :: [(String, (Int, Int))]
        rest = concat (map (help l) c)
        (TowerF _ _ c) = unFix t
    nameAndWeight (TowerF n w _) = ([n], w)
    weights = map (nameAndWeight . unFix . towerWeight)
    counts c = M.toList (foldr count M.empty (weights c))
    count k m = M.insertWith combine (snd k) (fst k, 1) m
    combine :: ([String], Int) -> ([String], Int) -> ([String], Int)
    combine (s1, i1) (ss, i) = (s1 ++ ss, i + i1)

main :: IO ()
main = do
  s <- getInput 7
  let table = parse s
  let (name, (targetWeight, stackWeight)) = minimumBy (comparing (fst . snd)) $ searchForUnbalanced $ baseTower $ makeTowerFStack table
  let (TowerF _ currWeight _) = fromJust (M.lookup name table)
  print $ (targetWeight - stackWeight) + currWeight

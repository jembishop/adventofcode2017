{-# LANGUAGE QuasiQuotes #-}

import AocUtils (getInput)
import Data.Fix (Fix, unfoldFix, unFix)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Debug.Trace ()
import Data.List (maximumBy)
import Data.Ord (comparing)
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

preParse :: String -> TowerMap
preParse = M.fromList . map (\x -> let p = parseOne x; TowerF s _ _ = p in (s, p)) . lines


makeTowerFStack :: TowerMap -> [Tower]
makeTowerFStack tm = map (unfoldFix stringToTowerF) (M.keys tm)
  where
    stringToTowerF s = fromJust (M.lookup s tm)

parse :: String -> TowerMap
parse = undefined

towerLength :: Tower -> Int
towerLength = go 0 
  where 
      go :: Int -> Tower -> Int
      go ix t = case l of 
          [] -> ix
          otherwise -> maximum $ map (go (ix + 1)) l
          where 
            (TowerF s i l) = unFix t

towerName :: Tower -> String
towerName t = let (TowerF s _ _)= unFix t in s
main :: IO ()
main = do
  s <- getInput 7
  print $ towerName $ maximumBy (comparing towerLength) $ makeTowerFStack $ preParse $ s

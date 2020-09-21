{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import AocUtils (getInput)
import Control.Monad (guard)
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Set as S
import Data.Text (Text, pack, singleton, unpack)
import Data.Text.Read (hexadecimal)
import Debug.Trace
import KnotHash (hash)
import Numeric (readHex)
import Text.Printf (printf)
import Control.Monad.State.Lazy
    ( State, evalState, MonadState(put, get) )

main :: IO ()
main = do
  s <- fmap pack (getInput 14)
  print $ solution1 $ parse s
  print $ solution2 $ parse s
  return ()

parse :: Text -> [Text]
parse = rowHash

rowHash :: Text -> [Text]
rowHash h = map (\x -> h <> "-" <> ((pack . show) x)) [0 .. 127]

solution1 :: [Text] -> Int
solution1 = sum . map (length . filter id) . grid

grid :: [Text] -> [[Bool]]
grid = map hashToBin . map hash

hashToBin :: Text -> [Bool]
hashToBin = binStrToBools . (>>= hexCharToBin) . unpack

binStrToBools :: String -> [Bool]
binStrToBools = fmap (\case '1' -> True; '0' -> False)

fromRight :: Show a => Either a p -> p
fromRight e = case e of
  Right x -> x
  Left x -> error (show x)

hexCharToBin :: Char -> String
hexCharToBin = printf "%04b" . hexToInt . singleton
  where
    hexToInt :: Text -> Int
    hexToInt = fst . fromRight . hexadecimal

type Point = (Int, Int)

type DiskArray = M.Map Point Bool

toDiskArray :: [[Bool]] -> DiskArray
toDiskArray l = M.fromList $ do
  (oIdx, nl) <- zip [0 ..] l
  (iIdx, v) <- zip [0 ..] nl
  return ((oIdx, iIdx), v)

type ZoneS = State ([Point], S.Set Point) (Maybe (S.Set Point))

findZoneStep :: DiskArray -> ZoneS
findZoneStep da = do
    (toExplore, visited) <- get
    let new = catMaybes $ map (toAdd visited da) toExplore
    let newExplore = new >>= getAdjacent
    let newVisited = S.union visited (S.fromList new)
    put (newExplore, newVisited)
    return $ if newExplore == [] then Just(visited) else Nothing

findZone :: DiskArray -> Point -> S.Set Point
findZone da p = fromJust $ fromJust $ find (/=Nothing) $ evalState (sequence (repeat (findZoneStep da))) ([p], S.empty)

toAdd :: (S.Set Point) -> DiskArray -> Point -> Maybe Point
toAdd visited da point = do
  v <- M.lookup point da
  guard v
  guard (not (S.member point visited))
  return point

allZones :: DiskArray -> [S.Set Point]
allZones da = filter ((/=0) . length) $ go S.empty (S.fromList (M.keys da)) []
  where
    go seen remaining sets = case S.minView remaining of
      Nothing -> sets
      Just ((next, rest)) -> go (rest <> nextZone) (S.difference rest nextZone) (nextZone:sets)
        where nextZone = findZone da next

solution2 :: [Text] -> Int
solution2 = length . allZones . toDiskArray . grid 

getAdjacent :: Point -> [Point]
getAdjacent (x, y) = [(x + 1, y), (x -1, y), (x, y + 1), (x, y -1)]

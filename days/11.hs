{-# LANGUAGE OverloadedStrings, LambdaCase #-}

import AocUtils (getInput)
import qualified Data.Text as T
import Data.List (inits)

main :: IO ()
main = do
  s <- fmap T.pack (getInput 11)
  print $ solution1 $ parse s
  print $ solution2 $ parse s


newtype Point = Point (Int, Int) deriving Show

instance Semigroup Point where
    (<>) (Point (x1, y1)) (Point (x2, y2)) = Point (x1+x2, y1+y2)

instance Monoid Point where
    mempty = Point (0, 0)

parse :: T.Text -> [Point]
parse = map (Point . go) . T.split (==',')
    where go = \case 
            "ne" -> (1, 1)
            "n" -> (0, 2)
            "nw" -> (-1, 1)
            "se" -> (1, -1)
            "s" -> (0, -2)
            "sw" -> (-1, -1)

coords :: [Point] -> Point
coords = mconcat

distance :: Point -> Int
distance (Point (x, y)) = 
    if diff > 0 then
        ax + ((ay - ax) `div` 2)
    else ax
    where ay = abs y
          ax = abs x
          diff = ay - ax

solution1 :: [Point] -> Int
solution1 = distance . coords
solution2 :: [Point] -> Int
solution2 = fst . foldl go (0, mempty)
    where go (dist, coord) dir = (max dist (distance new), new)
           where new = mappend coord dir

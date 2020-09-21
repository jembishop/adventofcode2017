{-# LANGUAGE OverloadedStrings #-}

import AocUtils (getInput)
import qualified Data.Attoparsec.Text as P
import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Text as T

main :: IO ()
main = do
  s <- fmap T.pack (getInput 13)
  print $ solution1 $ parse s
  print $ solution2 $ parse s

fromRight :: Show a => Either a p -> p
fromRight (Right x) = x
fromRight (Left x) = error (show x)

type Firewall = [(Int, Int)]

parse :: T.Text -> Firewall 
parse = map (fromRight . (P.parseOnly par)) . T.lines
  where
    par = do 
        depth <- P.decimal
        P.string ": "
        range <- P.decimal
        return (depth, range)

solution1 :: Firewall -> Int
solution1 = severity 0 

solution2 :: Firewall -> Int
solution2 fw = fst $ fromJust $ find (not . any id . snd) (map (\x -> (x, detections x fw)) [0..])

detections :: Int -> Firewall -> [Bool]
detections i = map go 
    where go = (==0) .pos
          pos (d, r) = if y < r then y else (2*r - y - 2) 
            where y = ((i + d) `mod` (2*r - 2))


severity :: Int -> Firewall -> Int
severity i fw = sum $ map ((uncurry (*)) . snd) $ filter fst $ zip (detections i fw) fw
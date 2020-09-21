{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import AocUtils (getInput)
import Control.Monad (guard)
import Control.Monad.State.Lazy
  ( MonadState (get, put),
    State,
    evalState,
  )
import qualified Data.Attoparsec.Text as P
import Data.Bits (Bits ((.&.)), shiftR)
import Data.Char (isDigit)
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

main :: IO ()
main = do
  s <- fmap pack (getInput 15)
  print $ solution1 (40*10^6) (parse s)
  print $ solution2  (5*10^6) (parse s)

fromRight :: Show a => Either a p -> p
fromRight (Right x) = x
fromRight (Left x) = error (show x)

parse :: Text -> (Integer, Integer)
parse = fromRight . (P.parseOnly par)
  where
    par = do
      P.skipWhile (not . isDigit)
      x <- P.decimal
      P.skipWhile (not . isDigit)
      y <- P.decimal
      return (x, y)

type Generator = (Integer, Integer)

gen1 :: Generator
gen1 = (16807, 2147483647)

gen2 :: Generator
gen2 = (48271, 2147483647)

step1 :: (Integer, Integer) -> (Integer, Integer)
step1 (i, j) = ((generate gen1) i, (generate gen2) j)

judgePairs :: ((Integer, Integer) -> (Integer, Integer)) -> Int -> (Integer, Integer) -> Int
judgePairs step n x = length $ filter id $ map judge $ take n (iterate step x)

generate :: Generator -> Integer -> Integer
generate (factor, divisor) prev = (prev * factor) `mod` divisor

generate2 :: Integer -> Generator -> Integer -> Integer
generate2 i g x = if (val `mod` i) /= 0 then generate2 i g val else val
  where
    val = generate g x

first :: Integer -> Integer
first i = 0xFFFF .&. i

judge :: (Integer, Integer) -> Bool
judge (i, j) = (first i) == (first j)

step2 :: (Integer, Integer) -> (Integer, Integer)
step2 (i, j) = ((generate2 4 gen1) i, (generate2 8 gen2) j)

solution1 :: Int -> (Integer, Integer) -> Int
solution1 = judgePairs step1

solution2 :: Int -> (Integer, Integer) -> Int
solution2 = judgePairs step2
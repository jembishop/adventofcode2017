{-# LANGUAGE OverloadedStrings #-}

import AocUtils (getInput)
import Control.Monad.State.Lazy
  ( MonadState (get, put),
    State,
    execState,
  )
import Text.Printf (printf)
import Data.Bits (xor)
import Data.Char (ord)
import Data.List.Split (chunksOf)
import qualified Data.Text as T
import Numeric (showHex)

main :: IO ()
main = do
  s <- fmap T.pack (getInput 10)
  print $ solution1 $ parse1 s
  print $ solution2 $ parse2 s

parse1 :: T.Text -> [Int]
parse1 = map (read . T.unpack) . T.split (== ',')

parse2 :: T.Text -> [Int]
parse2 = (++ [17, 31, 73, 47, 23]) . map ord . T.unpack

rotate :: Int -> [a] -> [a]
rotate n l = take (length l) $ drop n (cycle l)

aRotated :: ([a] -> [a]) -> Int -> [a] -> [a]
aRotated f i l = (rotate (length l - i) . f . rotate i) l

reverseN :: Int -> [a] -> [a]
reverseN i l = let (f, s) = splitAt i l in (reverse f) ++ s

type ListS = State (Int, Int, [Int]) ()

step :: Int -> ListS
step len = do
  (pos, ss, list) <- get
  let newList = aRotated (reverseN len) pos list
  put ((pos + ss + len) `mod` (length list), ss + 1, newList)

solution1 :: [Int] -> Int
solution1 l = let (_, _, (x : y : rest)) = (runRound l (0, 0, [0 .. 255])) in x * y

runRound :: [Int] -> (Int, Int, [Int]) -> (Int, Int, [Int])
runRound = execState . sequence . map step

fpow :: Int -> (b -> b) -> b -> b
fpow n f = foldr (.) id $ replicate n f

thrd :: (a, b, c) -> c
thrd (_, _, z) = z

solution2 :: [Int] -> String
solution2 l = (concat . map (printf "%02x"). map (foldr xor 0) . chunksOf 16 . thrd . (fpow 64 (runRound l))) (0, 0, [0 .. 255])

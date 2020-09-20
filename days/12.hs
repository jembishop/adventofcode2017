{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import AocUtils (getInput)
import qualified Data.Attoparsec.Text as P
import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Text as T
import Control.Applicative ( Alternative((<|>)) )
import qualified Data.Map as M
import Debug.Trace
import Control.Monad.State.Lazy
    ( State, evalState, MonadState(put, get) )

main :: IO ()
main = do
  s <- fmap T.pack (getInput 12)
  print $ solution1 $ parse s
  print $ solution2 $ parse s

type Graph = M.Map Int [Int]

fromRight :: Either a b -> b
fromRight (Right x) = x

parse :: T.Text -> Graph
parse = M.fromList . map (fromRight . (P.parseOnly par)) . T.lines
  where
    par = do 
        node <- P.decimal
        P.space
        P.string "<->"
        P.space
        neigh <- P.sepBy (P.decimal) (do {P.char ','; P.space}) <|> (pure <$> P.decimal)
        return (node, neigh)

       

type BfsS = State (Int, S.Set Int, [Int]) (Maybe (S.Set Int))


bfs :: Graph -> BfsS
bfs graph = do
    (start, visited, toVisit) <- get
    let newToVisit = toVisit ++ (filter (\x -> not (S.member x visited)) (fromJust (M.lookup start graph)))
    case newToVisit of 
        [] -> return (Just visited)
        (x:xs) -> do 
            put (x, (S.insert x visited), xs)
            return Nothing

connected :: Int -> Graph -> S.Set Int
connected i gr = fromJust $ fromJust $ find (/=Nothing) $ evalState (sequence (repeat (bfs gr))) (i, S.empty, [])

solution1 :: Graph -> Int 
solution1 = length . connected 0


solution2 :: Graph -> Int
solution2 g = go (S.empty) (S.fromList (M.keys g)) 0
    where go groupMems nonGroupMems acc = 
            case S.minView nonGroupMems of
            Just (x, _) -> go (S.union new groupMems) (S.difference nonGroupMems new) (acc + 1)
                            where new = connected x g
            Nothing -> acc

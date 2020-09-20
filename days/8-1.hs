import AocUtils (getInput)
import Control.Exception (Exception, throw)
import Control.Monad.State.Lazy
    ( State, execState, MonadState(put, get) )
import Data.List ( maximumBy )
import qualified Data.Map as M

main :: IO ()
main = do
  s <- getInput 8
  print $ solution $ parse s

type Ins = Int -> Int

type Cond = Int -> Bool

type Line = (String, Ins, String, Cond)

parseIns :: String -> Int -> Ins
parseIns ins i = case ins of
  "inc" -> (+ i)
  "dec" -> (subtract i)

parseCond :: String -> Int -> Cond
parseCond cond i = case cond of
  ">" -> (> i)
  "<" -> (< i)
  ">=" -> (>= i)
  "<=" -> (<= i)
  "==" -> (== i)
  "!=" -> (/= i)

parse :: String -> [Line]
parse = map (convert . words) . lines
  where
    convert [r1, ins, i, _, r2, cond, j] = (r1,  parseIns ins ((read i) :: Int), r2, (parseCond cond ((read j) :: Int)))


type Regs = (M.Map String Int) 
type RegsS = State Regs () 

getOrInsert:: String -> (M.Map String Int) -> (RegsS, Int)
getOrInsert r regs = case M.lookup r regs of
  Just (v) -> (return (), v)
  Nothing -> (put (M.insert r 0 regs), 0)

runStep :: Line -> RegsS
runStep (r1, ins, r2, cond) = do
  regs <- get
  let (action, res1) = getOrInsert r1 regs
  action
  let (act, res2) = getOrInsert r2 regs
  action
  let newVal = if cond res2 then ins res1 else res1
  put $ M.insert r1 newVal regs

runLines :: [Line] -> Regs
runLines lines = execState (sequence (map runStep lines)) M.empty

solution :: [Line] -> Int
solution = maximum . runLines 
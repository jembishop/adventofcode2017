module AocUtils (getInput) where	

getInput :: Int -> IO String
getInput dayNum = readFile $ "inputs/" ++ (show dayNum) ++ ".txt"

{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving#-}
import AocUtils (getInput)
import Control.Applicative

main :: IO ()
main = do
  s <- getInput 9
  print $ fmap solution1 $ parse s
  print $ fmap solution2 $ parse s

data Expr = Group [Expr] | Garbage String deriving Show

newtype Parser a = Parser {getParser :: (String -> Maybe (a, String)) } deriving Functor

instance Applicative Parser where
    pure x = Parser $ \s -> Just (x, s)
    p1 <*> p2 = Parser $ \s -> do
        (f, str) <- (getParser p1) s
        (res, str) <- ((getParser p2) str)
        return ((f res), str)

instance Alternative Parser where 
    empty = Parser $ \s -> Nothing 
    p1 <|> p2 = Parser $ \s -> ((getParser p1) s) <|> ((getParser p2) s)

satisfyP :: (Char -> Bool) -> Parser Char
satisfyP pred = Parser $ \s-> 
    case s of 
        (ch:str) -> if pred ch then Just (ch, str) else Nothing
        _ -> Nothing

cancelP :: Parser ()
cancelP = (fmap (const ()) $ some $ satisfyP (=='!') <* satisfyP (const True)) <|> pure ()

satCanP :: (Char -> Bool) -> Parser Char
satCanP pred = cancelP *> satisfyP pred

charP :: Char -> Parser Char
charP c = satCanP (==c)

garbageP :: Parser Expr
garbageP = fmap Garbage $ charP '<' *> many (satCanP (/='>')) <* charP '>'

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (fmap (:) element <*> many (sep *> element)) <|> pure []

groupP :: Parser Expr
groupP = fmap Group $ charP '{' *> sepBy (charP ',') exprP <* charP '}'

exprP :: Parser Expr
exprP = groupP <|> garbageP

parse :: String -> Maybe Expr 
parse s = do 
    (e, str) <- (getParser exprP) s
    if str /= "" then Nothing else Just e


solution1 :: Expr -> Int 
solution1 = go 1 0 
    where go level cum e = case e of
            Garbage _ -> cum
            Group gs  -> cum + level + (foldr (+) 0 (map (go (level + 1) 0) gs))

solution2 :: Expr -> Int 
solution2 = go 0 
    where go cum e = case e of
            Garbage s -> length s
            Group gs  -> cum + foldr (+) 0 (map (go 0) gs)
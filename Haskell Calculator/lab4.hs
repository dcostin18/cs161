
import Data.Char
import Data.List

type Number = Int
data ArithExpr = Number Int | Plus ArithExpr ArithExpr | Mult ArithExpr ArithExpr deriving (Show)

eval :: ArithExpr -> Int
eval (Number a) = a
eval (Plus x y) = (eval x) + (eval y)
eval (Mult x y) = (eval x) * (eval y)

spaceFree :: String -> String
spaceFree [] = []   
spaceFree (c:cs) 
	  | isSpace c = spaceFree cs
	  | otherwise = c : spaceFree cs

simpleParseExpr :: String -> [String]
simpleParseExpr [] = []
simpleParseExpr (c:cs)
		| isDigit c = (takeWhile isDigit (c:cs)) : simpleParseExpr (dropWhile isDigit (c:cs))
		| otherwise = (c:[]) : simpleParseExpr cs

buildExpr :: [String] -> ArithExpr
buildExpr [] = Number 0
buildExpr ls 
	  | length ls == 1 = Number (read (ls!!0))
	  | elemIndex "+" ls == Nothing = Mult (buildExpr $ fst tupm) (buildExpr $ drop 1 $ snd tupm )
	  | otherwise = Plus (buildExpr $ fst tup) (buildExpr $ drop 1 $ snd tup)
	  where
		tup = break (=="+") ls
		    tupm = break (=="*") ls

main :: IO ()
main = do
     putStrLn "Input: "
     hline <- getLine
     let line = hline
     putStrLn $ "Here's your result: " ++ (show (eval $ buildExpr $ simpleParseExpr $ spaceFree $ line))


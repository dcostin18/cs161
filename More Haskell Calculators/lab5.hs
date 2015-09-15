
import Data.Char
import Control.Applicative
import Data.List

data RArithExpr = Number Int | Frac RArithExpr RArithExpr | Plus RArithExpr RArithExpr | Mult RArithExpr RArithExpr | Var String 
data VRArithExpr a = Variable a | RArithExpr  
type PolyExpr = RArithExpr

instance Show RArithExpr where
	show (Number a) = show a
	show (Frac a b) = (show a) ++ "/" ++ (show b)
	show (Plus a b) = (show a) ++ "+" ++ (show b)
	show (Mult a b) = (show a) ++ "*" ++ (show b)
	show (Var a) = a

instance Functor VRArithExpr where
	fmap f (Variable a) = Variable (f a)

instance Applicative VRArithExpr where
	pure = Variable
	(Variable f) <*> (Variable x) = Variable (f x)

eval :: RArithExpr -> Int
eval (Number a) = a
eval (Plus x y) = (eval x) + (eval y)
eval (Mult x y) = (eval x) * (eval y)

evalRational :: RArithExpr -> RArithExpr
evalRational (Plus (Frac a b) (Frac c d)) = reduce (eval (Plus(Mult a d) (Mult b c))) (eval(Mult b d))
evalRational (Plus (Frac a b) (Number c)) = reduce (eval (Plus(Mult a (Number 1)) (Mult b (Number c)))) (eval(Mult b (Number 1)))
evalRational (Plus (Number a) (Frac c d)) = reduce (eval (Plus(Mult (Number a) d) (Mult (Number 1) c))) (eval(Mult (Number 1) d))

evalRational (Mult (Frac a b) (Frac c d)) = reduce (eval(Mult a c)) (eval(Mult b d))
evalRational (Mult (Number a) (Frac c d)) = reduce (eval(Mult (Number a) c)) (eval(Mult (Number 1) d))
evalRational (Mult (Frac a b) (Number c)) = reduce (eval(Mult a (Number c))) (eval(Mult b (Number 1)))
evalRational (Plus a b) = Number(eval(Plus a b))
evalRational (Mult a b) = Number(eval(Mult a b))
evalRational (Frac a b) = Frac a b
 
reduce :: Int -> Int -> RArithExpr                                                                                                                         
reduce a b = Frac (Number (a `div` (gcd2 (abs a) (abs b)))) (Number(b `div` (gcd2 (abs a) (abs b)))) where
	gcd2 a 0 = a
	gcd2 a b = gcd2 b (a `rem` b)

collect :: RArithExpr -> RArithExpr
collect (Mult a (Var x)) = Mult a (Var x)
collect (Mult a (Mult b (Var x))) = Mult (Mult a b) (Var x)
collect (Plus (Mult a (Var x)) (Mult b (Var y))) = Mult (Plus a b) (Var x)
collect (Plus a (Var x)) = (Plus a (Var x))

collect (Mult (Var x) a) = Mult a (Var x)
collect (Mult (Mult b (Var x)) a ) = Mult (Mult a b) (Var x)
collect (Plus (Var x) a ) = (Plus a (Var x))

collect (Plus a b) = Plus (collect a) (collect b)
collect (Mult a b) = Mult (collect a) (collect b)

spaceFree :: String -> String
spaceFree [] = [] 	
spaceFree (c:cs) 
	| isSpace c = spaceFree cs
	| otherwise = c : spaceFree cs


simpleParseExpr :: String -> [String]
simpleParseExpr [] = []
simpleParseExpr (c:cs)
	| isDigit c = (takeWhile isDigit (c:cs)) : simpleParseExpr (dropWhile isDigit (c:cs))
	| isAlpha c = "*" : (takeWhile isAlpha (c:cs)) : simpleParseExpr (dropWhile isAlpha (c:cs))
	| otherwise = (c:[]) : simpleParseExpr cs

buildExpr :: [String] -> RArithExpr
buildExpr [] = Number 0
buildExpr ls 
	| length ls == 1 && all isDigit (ls!!0) = Number (read (ls!!0))
	| length ls == 1 && all isAlpha (ls!!0) = Var (ls!!0)
	| elemIndex "+" ls == Nothing = 
		if elemIndex "*" ls == Nothing then Frac (buildExpr $ fst tupd) (buildExpr $ drop 1 $ snd tupd)
			else Mult (buildExpr $ fst tupm) (buildExpr $ drop 1 $ snd tupm)
	| otherwise = Plus (buildExpr $ fst tup) (buildExpr $ drop 1 $ snd tup)
	where
		tup = break (=="+") ls
		tupm = break (=="*") ls
		tupd = break (=="/") ls

main :: IO ()
main = do
	putStrLn "Input: "
	hline <- getLine
	let line = hline
	putStrLn $ "Here's your result: " ++ (show (evalRational $ buildExpr $ simpleParseExpr $ spaceFree $ line))


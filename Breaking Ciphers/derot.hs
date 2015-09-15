import Data.Char
import System.Exit
import System.Environment
import System.IO
import Data.List

main = do

	handle <- openFile "dictionary.txt" ReadMode
	dictWords <- fmap words (hGetContents handle)
	let dict = map (convertAllToUpper) dictWords
	fhandle <- openFile "c1.txt" ReadMode
	fline <- hGetLine fhandle
	let line = fline
	let wordsLs = map (convertAllToUpper)( words line)

	putStr "n = "
	let n = decode wordsLs dict 25
	putStrLn $ show $ n
	putStrLn $ lsToString $ map (rot n) wordsLs

	hClose handle

decode :: [String] -> [String] -> Int -> Int
decode wrds dict 0 = 0
decode wrds dict n
	| valid decodedWords dict = n
	| otherwise = decode wrds dict (n-1)
	where
		decodedWords = map (rot n) wrds

validateInt :: String -> Bool
validateInt "" = False
validateInt (c:cs) = (isDigit c || c == '-') && all isDigit cs

valid :: Eq a => [a] -> [a] -> Bool
valid wrds dict 
	| inDict (wrds !! 0) && inDict (wrds !! 1) = True
	| otherwise = False
	where
		inDict word
			| elemIndex word dict /= Nothing = True
			| otherwise = False

rot :: Int -> String -> String
rot n = map (rotChar) where
    rotChar c
        | isLower c = rotCase 'a' c n
        | isUpper c = rotCase 'A' c n
        | otherwise = c
    rotCase cas c n = chr (ord cas + (ord c - ord cas + n)  `mod` 26)

convertAllToUpper :: [Char] -> [Char]
convertAllToUpper [] = []
convertAllToUpper (c:cs) = 
	if ((ord c) > 96 && (ord c) < 123) then (chr ((ord c) - 32)) : convertAllToUpper cs
	else (c : convertAllToUpper cs)
	
lsToString :: [[Char]] -> [Char]
lsToString [] = ""
lsToString (c:cs) = (c ++ " ") ++ lsToString cs

usage :: IO ()
usage = do
	progname <- getProgName
	hPutStrLn stderr $ "usage : " ++ progname ++ " [n]"
	exitWith $ ExitFailure 255

import Data.Char
import System.Exit
import System.Environment
import System.IO
import Data.List
import Data.Ord

main = do
	handle <- openFile "dictionary.txt" ReadMode
	dictWords <- fmap words (hGetContents handle)
	let dict = listToUpper dictWords
	fhandle <- openFile "c2.txt" ReadMode
	fline <- hGetLine fhandle
	let line' = fline
	let line = convertAllToUpper line'
	let words16 = listToUpper ( extract16 dict )
	let words6 = listToUpper (extract6 dict)

	--An example that works: 
	let example = "ZTMKVED BM FLG" 
	let pass2 = vdecode example dict words6 0
	putStrLn $ "Password: " ++  pass2
	putStrLn $ "Decoded message: " ++ vindecode pass2 example 0

	--These lines theoretically decode any 16-char Vigenere cipher
	let pass = vdecode line dict words16 0
	putStrLn $ pass
	putStrLn $ vindecode pass line 0
	hClose handle

--Cracks the code
vdecode :: [Char] -> [String] -> [[Char]] -> Int -> [Char]
vdecode string dict possWords pindex
	| pindex ==length possWords = "Error: No valid decryption key found"
	| valid decryptWords dict = (possWords!!pindex)
	| otherwise = vdecode string dict possWords (pindex+1)
	where
		decryptWords = words (vindecode (possWords!!pindex) string 0)

--Applies in reverse the password onto the string
vindecode :: [Char] -> [Char] -> Int -> [Char]
vindecode _ [] _ = []
vindecode key (c:cs) index 
	| ((ord c)<64 || (ord c) > 123) = c : (vindecode key cs (index)) 
	| otherwise = (rotChar c n) : (vindecode key cs (index+1)) 
	where
		n = 65 - ( ord (key!!(index `mod` (length key)) ))

validWord :: Eq a => a -> [a] -> Bool
validWord word dict
	| (elemIndex word dict /= Nothing) = True
	| otherwise = False

valid :: Eq a => [a] -> [a] -> Bool
valid wrds dict 
	| (validWord (wrds !! 1) dict) && (validWord (wrds !! 2) dict) = True
	| otherwise = False

rotChar :: Char -> Int -> Char
rotChar c n
    | isLower c = rotCase 'a' c n
	| isUpper c = rotCase 'A' c n
    | otherwise = c 
    where
    	rotCase cas c n = chr (ord cas + (ord c - ord cas + n)  `mod` 26)

convertAllToUpper :: [Char] -> [Char]
convertAllToUpper = map toUpper

listToUpper :: [[Char]] -> [[Char]]
listToUpper = map convertAllToUpper

extract16 :: [[a]] -> [[a]]
extract16 [] = []
extract16 (c:cs)  
	| length c == 16 = c : extract16 cs
	| otherwise = extract16 cs

extract6 :: [[a]] -> [[a]]
extract6 [] = []
extract6 (c:cs)  
	| length c == 6 = c : extract6 cs
	| otherwise = extract6 cs

data Tree = Leaf Int | Node Int Tree Tree deriving (Show, Eq)



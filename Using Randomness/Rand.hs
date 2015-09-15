import RandState	
import UCState
import Prelude 
import System.Random
import StateExample

data CardValue = King | Queen | Jack | NumberCard Int
	deriving (Show, Eq)
data CardSuit = Hearts | Diamonds | Spades | Clubs
	deriving (Show, Eq)
data PlayingCard = PlayingCard CardSuit CardValue
	deriving (Show, Eq)

fullCardDeck :: [PlayingCard]
fullCardDeck = [PlayingCard s v | s <- allsuits, v <- allvals] where
	allvals = King : Queen : Jack : [NumberCard i | i <- [1..10]]
	allsuits = [Hearts, Diamonds, Spades, Clubs]

--PROBLEM 1
testStateStateExample2 :: Int -> IO Bool
testStateStateExample2 n = do
    ls <- stateExample2 n
    return $ n == length ls

--PROBLEM 2
testAll :: IO Int
testAll = do
	b1 <- testShuffleADeck 
	b2 <- testShuffleDeck
	b3 <- testRemoveCard
	b4 <- testRollTwoDice
	b5 <- testStateStateExample2 15
	let ls = [b1, b2, b3, b4, b5]
	return (countBools ls) where
		countBools :: [Bool] -> Int
		countBools [] = 0
		countBools (c:cs) = case c of
			True -> 1+(countBools cs)
			False -> countBools cs

--PROBLEM 3
randR :: Random a => (a, a) -> RandState a
randR (hi, lo)= do
	gen <- get
	let (x, gen') = randomR (hi,lo) gen
	put gen'
	return x

--PROBLEM 4
rollTwoDice :: IO Int
rollTwoDice = do
	gen <- newStdGen
	gen' <- newStdGen
	let x = runRandom (randR (1,6)) gen :: Int
	let y = runRandom (randR (1,6)) gen' :: Int
	return $ x + y

--(I chose 7 cases to achieve some degree of statistical significance)
testRollTwoDice :: IO Bool
testRollTwoDice = do
	case1 <- rollTwoDice
	case2 <- rollTwoDice
	case3 <- rollTwoDice
	case4 <- rollTwoDice
	case5 <- rollTwoDice
	case6 <- rollTwoDice
	case7 <- rollTwoDice
	let list = [case1, case2, case3, case4, case5, case6, case7]
	return (foldr (&&) True (map (\x -> x<13 && x>0) list))

--PROBLEM 5
removeCard :: [a] -> IO (a, [a])
removeCard stack = do
	gen <- newStdGen
	let rem_card = (runRandom (randR (0,(length stack - 1))) gen :: Int)
	return (stack!!rem_card, remove rem_card stack)
	where
		remove index ls = (take (index) ls) ++ (drop (index+1) ls)

testRemoveCard :: IO Bool
testRemoveCard = do
	let one = PlayingCard Hearts Jack
	let two = PlayingCard Spades (NumberCard 4)
	let three = PlayingCard Hearts (NumberCard 9)
	let list = [one, two, three]
	(a,b) <- removeCard list
	let b1 = a == one || a == two || a == three
	let b2 = length b == (length list -1)
	return (b1 && b2)

--PROBLEM 6
shuffleDeck :: [a] -> IO [a]
shuffleDeck stack = shuffleDeck' stack [] where
	shuffleDeck' [] ls = return ls
	shuffleDeck' list base = do 
		rem <- removeCard list
		shuffleDeck' (snd rem) ((fst rem):base) 

testShuffleDeck :: IO Bool
testShuffleDeck = do
	l1 <- shuffleDeck fullCardDeck
	l2 <- shuffleDeck fullCardDeck
	return (l1 /= l2)

--PROBLEM 7
shuffleADeck :: IO [PlayingCard]
shuffleADeck = shuffleDeck fullCardDeck

--This should return false less than (52 nPr 52) times = 1 in 10^67 times
testShuffleADeck :: IO Bool
testShuffleADeck = do
	l1 <- shuffleADeck
	return (length l1 == 52)






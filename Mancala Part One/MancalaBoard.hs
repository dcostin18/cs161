module MancalaBoard (MancalaBoard, Player, initial, getCurPlayer,
            getBoardData, numCaptured, move, allowedMoves, isAllowedMove,
            gameOver, winners) where

import Data.List as List -- for List.elemIndex
import Data.Maybe as Maybe -- for List.elemIndex

{-
 - The stones on a Mancala board are simply recorded as a list of Ints.  The
 -  Ints come in the following order:
 - 1. The boardSize pits belonging to PlayerA
 - 2. The store belonging to PlayerA
 - 3. The boardSize pits belonging to PlayerB
 - 4. The store belonging to PlayerB
 -}

data MancalaBoard = MancalaBoardImpl [Int] Player

data Player = PlayerA | PlayerB deriving (Eq, Show)

---- Functions/constants for Player ----

allPlayers = [PlayerA, PlayerB]
numPlayers = length allPlayers


playerNum :: Player -> Int
playerNum p = fromJust $ List.elemIndex p allPlayers


playerWithNum :: Int -> Player
playerWithNum i = allPlayers !! i


nextPlayer :: Player -> Player
{- Find the player whose turn is next -}
nextPlayer p = playerWithNum $ ((playerNum p) + 1) `mod` numPlayers


---- Functions/constants for MancalaBoard ----

{- number of pits on each side -}
boardSize = 6
{- number of stones in each pit -}
startStones = 4

{- the initial mancala board -}
initial :: MancalaBoard
initial = MancalaBoardImpl (concat $ take numPlayers (repeat boardSide)) PlayerA
                        -- One side of board                pit at end
    where boardSide = take boardSize (repeat startStones) ++ [0]


{- return the index of the first pit belonging to a player -}
indexForFirstPit :: Player -> Int
indexForFirstPit p = (playerNum p) * (boardSize + 1)


{- return the index of the store for that player -}
indexForPlayerStore :: Player -> Int
indexForPlayerStore p = boardSize + (indexForFirstPit p)


{- return the indices for the pits (without the store) for a player -}
indicesForPlayerSide :: Player -> [Int]
indicesForPlayerSide p = [firstPit .. lastPit] where
    firstPit = indexForFirstPit p
    lastPit = firstPit + boardSize - 1


---- Retrieve information about Mancala Board
-- TODO: uncomment these type declarations and implement the functions
{- return the player who has the current turn -}
getCurPlayer :: MancalaBoard -> Player
--TODO: replace below line with real definition
getCurPlayer (MancalaBoardImpl ls play) = play


{- return the list of all pits in the board -}
getBoardData :: MancalaBoard -> [Int]
--TODO: replace below line with real definition
getBoardData (MancalaBoardImpl ls play) = ls


{- TDODO: return the side of the board for a specified player, including the store at
 - the end -}
--TODO: define this function
playerSide :: MancalaBoard -> Player -> [Int]
playerSide (MancalaBoardImpl ls play) player = getList (indicesForPlayerSide player ++ (indexForPlayerStore player : [])) where
	getList [] = []
	getList (c:cs) = ls!!c : getList cs



{- TODO: return the number of captured pieces in specified player's store -}
--TODO: add type and replace below line with real definition
numCaptured :: MancalaBoard -> Player -> Int
numCaptured (MancalaBoardImpl ls play) player = ls!!(indexForPlayerStore player)


{- TODO:  allowedMoves returns a list of valid moves for the current player:
 - ie. the indices of pits which belong to that player, and which contain one
 - or more pieces -}
--TODO: add type and replace below line with real definition
allowedMoves :: MancalaBoard -> [Int]
allowedMoves (MancalaBoardImpl ls play) = [x | x <- indicesForPlayerSide (getCurPlayer (MancalaBoardImpl ls play)), ls!!x>0  ]


{- TODO:  check that a move is valid for the current player -}
--TODO: add type and replace below line with real definition
isAllowedMove :: MancalaBoard -> Int -> Bool
isAllowedMove board pos = pos `elem` (allowedMoves board)


{- TODO: We number the pits from 0 to 13 (2 players, 6 pits each and 1 store each)
 - This function takes a board and applies the move where the player selects
 - the numbered pit, giving back an updated board after the move
  -}
--TODO: add type and replace below line with real definition

move :: MancalaBoard -> Int -> MancalaBoard
move (MancalaBoardImpl list p) pos = MancalaBoardImpl ((foldr (:) [] (map move' [0..13]))) p' where
	p' = nextPlayer p
	move' n
		| n == pos = 0
		| otherwise = if (n>pos && n < (pos+(list!!pos)+1)) then (list!!(n+1))
			else list!!n


{- TODO: gameOver checks to see if the game is over (i.e. if one player's side of the
 - board is all empty -}
gameOver :: MancalaBoard -> Bool
gameOver board = 
	if ((all (==0) (playerSide board curPlayer))||(all (==0) (playerSide board (nextPlayer curPlayer)))) then True
		else False where curPlayer = getCurPlayer board


{- TODO: winner returns a list of players who have the top score: there will only be 
 - one in the list if there is a clear winner, and none if it is a draw
  TODO output based on score -}
--winners :: MancalaBoard -> [Player]

winners :: MancalaBoard -> Player
winners board 
	| all (==0) (init side1) = if((last side1) > (foldr (+) 0 side2)) then curPlayer else (nextPlayer curPlayer)
	| otherwise = if((last side2) < (foldr (+) 0 side1)) then curPlayer else (nextPlayer curPlayer)
	where
		curPlayer = getCurPlayer board
		side1 = playerSide board curPlayer
		side2 = playerSide board (nextPlayer curPlayer)


---- show, TODO pretty print the board and game state
instance Show MancalaBoard where
    show (MancalaBoardImpl boardData player) =
            ((show (take 7 boardData)) ++ " " ++ (show PlayerA)) ++ "     " ++
            ((show (drop 7 boardData)) ++ " " ++ (show PlayerB))

testGetCurPlayer :: Bool
testGetCurPlayer = getCurPlayer board == PlayerA where
	board = MancalaBoardImpl [] PlayerA
testGetBoardData :: Bool
testGetBoardData = getBoardData board == [1,2,3] where
	board = MancalaBoardImpl [1,2,3] PlayerA
testPlayerSide :: Bool
testPlayerSide = playerSide board PlayerA == [1..7] where
	board = MancalaBoardImpl [1..14] PlayerA
testNumCaptured :: Bool
testNumCaptured = numCaptured board PlayerA == 7 where
	board = MancalaBoardImpl [1..14] PlayerA
testAllowedMoves :: Bool
testAllowedMoves = allowedMoves board == [0..5] where
	board = MancalaBoardImpl [1..14] PlayerA
testIsAllowedMove :: Bool
testIsAllowedMove = isAllowedMove board 3 == True where
	board = MancalaBoardImpl [1..14] PlayerA
testMove :: Bool
testMove = getBoardData (move board 3) == result where
	result =[0,1,2,0,5,6,7,8,9,10,11,12,13]
	board = MancalaBoardImpl [1..14] PlayerA
testGameOver :: Bool
testGameOver = gameOver board == False where
	board = MancalaBoardImpl [1..14] PlayerA
testWinners :: Bool
testWinners = winners board == PlayerB where
	board = MancalaBoardImpl ([1..14]++[800]) PlayerA




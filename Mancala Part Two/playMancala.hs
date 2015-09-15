import Data.List -- for List.elemIndex
import Data.Maybe -- for List.elemIndex

--CODE THAT I WROTE:
main :: IO ()
main = do
  let startBoard = initial
  prettyPrint initial
  play initial PlayerA

play :: MancalaBoard -> Player -> IO ()
play board player = 
  if ((not . gameOver) board) then
    do
      putStrLn $ (show player) ++ "'s turn, which pieces to move?"
      input <- getLine
      if (isAllowedMove board (read input)) then do  
        let newboard = move board (read input)
        prettyPrint newboard
        play newboard (nextPlayer player)
      else do
        putStrLn "Invalid move, please restart."
        play board player
  else do
    let wins = winners board
    if (length wins == 1) then putStrLn ((show (wins!!0)) ++ " wins!!!")
      else putStrLn "Draw!"


prettyPrint (MancalaBoardImpl boardData player) = mapM_ putStrLn [string1, string2, string3] where
  p n = (show (boardData!!n)) ++ " "
  string1 = "| " ++ (p 12) ++ (p 11)++ (p 10) ++ "    " ++ (p 9) ++ (p 8)++ (p 7) ++ " |"
  string2 = "|" ++ (p 13) ++ (replicate 14 ' ') ++ (p 6) ++ "|"
  string3 = "| " ++ (p 0) ++ (p 1)++ (p 2) ++ "    " ++ (p 3) ++ (p 4)++ (p 5) ++ " |"


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
playerNum p = fromJust $ Data.List.elemIndex p allPlayers


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
getCurPlayer (MancalaBoardImpl _ p) = p


{- return the list of all pits in the board -}
getBoardData :: MancalaBoard -> [Int]
getBoardData (MancalaBoardImpl boardData _) = boardData


{- return the side of the board for a specified player, including the store at
 - the end -}
playerSide :: MancalaBoard -> Player -> [Int]
playerSide (MancalaBoardImpl boardData _) player = [boardData !! i | i <- indicesForPlayerSide player]


{- return the number of captured pieces in specified player's store -}
numCaptured (MancalaBoardImpl boardData _) player = boardData !! (indexForPlayerStore player)


{- allowedMoves returns a list of valid moves for the current player:
 - ie. the indices of pits which belong to that player, and which contain one
 - or more pieces -}
allowedMoves :: MancalaBoard -> [Int]
allowedMoves mancala = allowedMovesFor mancala (getCurPlayer mancala)

allowedMovesFor :: MancalaBoard -> Player -> [Int]
allowedMovesFor (MancalaBoardImpl boardData _) player = [i | i <- indicesForPlayerSide player, (boardData !! i) /= 0]

{- check that a move is valid for the current player -}
isAllowedMove :: MancalaBoard -> Int -> Bool
isAllowedMove b i = i `elem` (allowedMoves b)



{- gameOver checks to see if the game is over (i.e. if one player's side of the
 - board is all empty -}
gameOver :: MancalaBoard -> Bool
-- Assume two players
-- The game is over if the last player ended his turn with an empty side
--      (it is sufficient to check that either side is empty, 
--        as the game must have ended this turn or the previous turn
--        if either side is empty)
gameOver b = (sum $ playerSide b PlayerA) == 0 ||
             (sum $ playerSide b PlayerB) == 0


{- winner returns a list of players who have the top score: there will only be 
 - one in the list if there is a clear winner, and none if it is a draw -}
winners :: MancalaBoard -> [Player]
-- assume two players, and that game is over
winners b
    | ascore > bscore = [PlayerA]
    | bscore > ascore = [PlayerB]
    | otherwise = [PlayerA, PlayerB] where 
          ascore = (sum $ playerSide b PlayerA) + (numCaptured b PlayerA)
          bscore = (sum $ playerSide b PlayerB) + (numCaptured b PlayerB)


---- show
instance Show MancalaBoard where
    show (MancalaBoardImpl boardData player) =
            (show boardData) ++ " " ++ (show player)



{- We number the pits from 0 to 13 (2 players, 6 pits each and 1 store each)
 - This function takes a board and applies the move where the player selects
 - the numbered pit, giving back an updated board after the move -}

{- One approach: build a function that tells you how many pieces
 - added to each else -}
move1 :: MancalaBoard -> Int -> MancalaBoard
move1 board i = MancalaBoardImpl updatedBoard nextPlayer' where
        (before, (n:after)) = splitAt i $ getBoardData board
        -- remove the seeds from the specified pit
        seedsRemoved = before ++ (0:after)
        -- Number of times it totally goes around board
        fullLaps = n `div` (totalBoardSize - 1)
        leftover = n `mod` (totalBoardSize - 1)
        
        -- Helper calculations
        totalBoardSize = (boardSize + 1) * numPlayers
        playerStore = indexForPlayerStore $ getCurPlayer board
        -- Assume two players
        opponentStore = indexForPlayerStore $ nextPlayer $ getCurPlayer board

        -- how many steps around board from selected pit are we
        stepsAway j
            | relPos < oppStoreRelPos = relPos
            | otherwise = relPos - 1 where 
            relPos = (j - i) `mod` totalBoardSize
            oppStoreRelPos = (opponentStore - j) `mod` totalBoardSize

        nextPlayer'
            | stepsAway playerStore == leftover = getCurPlayer board
            | otherwise = nextPlayer $ getCurPlayer board

        -- Number of pieces to add to position j
        addedPieces j
            | j == opponentStore = 0
            | stepsAway j > 0 && stepsAway j <= leftover = fullLaps + 1
            | otherwise = fullLaps

        updatedBoard = zipWith (+) seedsRemoved (
                        map addedPieces [0..totalBoardSize])


{- Second approach: use pickupStones to recursively go around board -}
-- assumes the move i is legal -- it's up to the user of MancalaBoard to check this first!!
move mancala i = MancalaBoardImpl newBoardData newPlayer where
                     boardData = getBoardData mancala
                     curPlayer = getCurPlayer mancala
                     curPlayerStoreIndex = indexForPlayerStore curPlayer
                     otherPlayerStoreIndex = indexForPlayerStore (nextPlayer curPlayer)
                     numStones = boardData !! i

                     midBoardData = fst $ pickupStones boardData i
                     newBoardData = placeStones midBoardData numStones ((i + 1) `mod` (length boardData)) otherPlayerStoreIndex
                     newPlayer = if (i + numStones == curPlayerStoreIndex) then curPlayer else (nextPlayer curPlayer)

---- internal helpers for move
pickupStones :: [Int] -> Int -> ([Int], Int)
pickupStones boardData 0 = ((0 : (tail boardData)), head boardData)
pickupStones boardData i = (((head boardData) : (fst rec)), snd rec) where rec = pickupStones (tail boardData) (i-1)

placeStones :: [Int] -> Int -> Int -> Int -> [Int]
placeStones boardData 0 _ _ = boardData
placeStones boardData numStones i exclIndex | i == exclIndex = placeStones boardData numStones ((i + 1) `mod` (length boardData)) exclIndex
                                            | otherwise = placeStones (listIncr boardData i) (numStones - 1) ((i + 1) `mod` (length boardData)) exclIndex

-- listIncr vals idx returns a new list with (vals !! idx) incremented by 1
listIncr :: [Int] -> Int -> [Int]
listIncr vals i | i > length vals = listIncr vals (i `mod` length vals)
                | i == 0 = ((head vals + 1) : (tail vals))
                | otherwise = (head vals) : (listIncr (tail vals) (i-1))
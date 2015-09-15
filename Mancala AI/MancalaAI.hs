module MancalaAI(aiNextMove) where

import MancalaBoard
import Data.List

type Move = Int

-- Decides what move to make for the current player of the specified MancalaBoard.
aiNextMove :: MancalaBoard -> Move
aiNextMove mancala = lookahead mancala 0

evalPosition :: Player -> MancalaBoard -> Int -> Int
evalPosition player mancala 0 = (getBoardData mancala)!!(last(playerSide mancala player))
evalPosition player mancala depth = evalPosition player (move mancala (lookahead mancala depth-1)) (depth-1)

lookahead :: MancalaBoard -> Int -> Move
lookahead mancala depth = allMoves!!(index max allBoardScores 0) where
	allMoves = allowedMovesFor mancala player
	allBoardScores = [evalPosition player (move mancala x) depth | x <- allMoves]
	max = maximum allBoardScores
	player = getCurPlayer mancala

index :: Eq a => a -> [a] -> Int -> Int
index elt ls ind 
	| ind == (length ls) - 1 = if (ls!!((length ls) -1) == elt) then ((length ls) - 1) else -1 
	| otherwise = if(ls!!ind == elt) then ind else (index elt ls (ind+1))


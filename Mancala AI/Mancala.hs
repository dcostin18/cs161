import MancalaBoard
import MancalaAI

isGameOver :: MancalaBoard -> Bool
isGameOver mancala = any (==0) (map (length . (allowedMovesFor mancala)) allPlayers)

playerA = allPlayers !! 0
playerB = allPlayers !! 1

playGame :: MancalaBoard -> IO ()
playGame mancala = 
  do
    -- show board and scores
    putStrLn ((show mancala) ++ " PlayerA: " ++ (show $ playerScore mancala playerA) ++ " PlayerB: " ++ (show $ playerScore mancala playerB))
    
    -- check if it's the AI's turn
    if (getCurPlayer mancala == playerB)     
      -- AI move
      then let nextMove = aiNextMove mancala in
        do 
        putStrLn ("Player B moved " ++ (show nextMove))
        continue (move mancala nextMove)
      -- Human move
      else do 
        putStrLn ("Select a move from the following: " ++ show (allowedMoves mancala))
        c <- getLine
        let nextMancala = (move mancala (read c)) in
          continue nextMancala

continue :: MancalaBoard -> IO ()
continue mancala = if isGameOver mancala
                     then endGame mancala
                     else playGame mancala

endGame :: MancalaBoard -> IO ()
endGame mancala = 
  do
    if length winners == 1
      then putStrLn (show (head winners) ++ " wins!")
      else putStrLn "Tie!" where
        winners = whoWins mancala

main :: IO ()
main = 
  do
    putStrLn "Welcome to Mancala!  Type 'main' to start playing.  Typing any invalid input will cause the game to stop :(."
    playGame initial
    putStrLn "Thanks for playing."

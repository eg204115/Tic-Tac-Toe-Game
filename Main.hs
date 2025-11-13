module Main where
import DataTypes
import Processing
import IOHandler
import Utils

main :: IO ()
main = gameLoop emptyBoard X

gameLoop :: Board -> Player -> IO ()
gameLoop board player = do
    displayBoard board
    putStrLn $ "Player " ++ show player ++ ", enter your move (row col):"
    input <- getLine
    let [r, c] = map read (words input)
    case makeMove board (r, c) player of
        Nothing -> do
            putStrLn "Invalid move! Try again."
            gameLoop board player
        Just newBoard ->
            case checkWin newBoard of
                Just p -> do
                    displayBoard newBoard
                    putStrLn $ "🎉 Player " ++ show p ++ " wins!"
                Nothing ->
                    if isDraw newBoard
                    then do
                        displayBoard newBoard
                        putStrLn "It's a draw!"
                    else
                        gameLoop newBoard (switchPlayer player)

switchPlayer :: Player -> Player
switchPlayer X = O
switchPlayer O = X

module IOHandler where
import DataTypes

displayCell :: Cell -> Char
displayCell Empty      = ' '
displayCell (Mark X)   = 'X'
displayCell (Mark O)   = 'O'

displayBoard :: Board -> IO ()
displayBoard board = do
    putStrLn "\nCurrent Board:"
    putStrLn "-------------"
    mapM_ (putStrLn . rowToStr) board
    putStrLn "-------------"

rowToStr :: [Cell] -> String
rowToStr row = "| " ++ unwords (map (\c -> [displayCell c]) row) ++ " |"

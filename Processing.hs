module Processing where
import DataTypes

-- Place a move on the board
makeMove :: Board -> (Int, Int) -> Player -> Maybe Board
makeMove board (r, c) player =
    if board !! r !! c == Empty
    then Just (take r board ++
               [take c (board !! r) ++ [Mark player] ++ drop (c+1) (board !! r)] ++
               drop (r+1) board)
    else Nothing

-- Check for a win
checkWin :: Board -> Maybe Player
checkWin board =
    case filter (/= Empty) (map (winner board) linesToCheck) of
        (Mark p:_) -> Just p
        _          -> Nothing

winner :: Board -> [(Int, Int)] -> Cell
winner board coords =
    let cells = [board !! r !! c | (r, c) <- coords]
    in if all (== head cells) cells then head cells else Empty

linesToCheck :: [[(Int, Int)]]
linesToCheck =
    [ [(0,0),(0,1),(0,2)], [(1,0),(1,1),(1,2)], [(2,0),(2,1),(2,2)] -- Rows
    , [(0,0),(1,0),(2,0)], [(0,1),(1,1),(2,1)], [(0,2),(1,2),(2,2)] -- Columns
    , [(0,0),(1,1),(2,2)], [(0,2),(1,1),(2,0)]                     -- Diagonals
    ]

isDraw :: Board -> Bool
isDraw = all (/= Empty) . concat

module Utils where
import DataTypes

emptyBoard :: Board
emptyBoard = replicate 3 (replicate 3 Empty)

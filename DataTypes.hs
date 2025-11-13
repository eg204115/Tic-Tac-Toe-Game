module DataTypes where

data Player = X | O deriving (Eq, Show)
data Cell = Empty | Mark Player deriving (Eq)
type Board = [[Cell]]

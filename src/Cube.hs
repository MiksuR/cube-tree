module Cube where

data Color = W | Y | G | B | R | O deriving (Show, Eq, Enum, Ord)
data Cubie = Edge Color Color | Corner Color Color Color deriving (Show)
type Orient = Int
type CubeTree = [[(Cubie, Orient)]]

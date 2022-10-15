module Cube where

import Data.Set (Set, fromList)

data Color = W | Y | G | B | R | O deriving (Show, Eq, Enum, Ord)
data Cubie = Edge !Color !Color |
             Corner !Color !Color !Color deriving (Show, Eq, Ord)
data CubieEmbed = CubieEmbed { cubie :: !Cubie,
                               location :: ![Int] } deriving (Show)
--                             location <- [0, 1, 2]^3
type Layer = [CubieEmbed]
type Cube = [Layer]

rotate :: Cubie -> Cubie
rotate (Edge a b) = Edge b a
rotate (Corner a b c) = Corner c a b

corners :: Set Cubie
corners = fromList [Corner W G O, Corner W O B, Corner W B R, Corner W R G,
                    Corner Y O G, Corner Y B O, Corner Y R B, Corner Y G R]
edges :: Set Cubie
edges = fromList [Edge W G, Edge W O, Edge W B, Edge W R,
                  Edge G O, Edge B O, Edge B R, Edge G R,
                  Edge Y G, Edge Y O, Edge Y B, Edge Y R]

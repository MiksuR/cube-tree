module Cube where

import Prelude hiding (Right, Left)
import Data.Set (Set, fromList)

data Color = W | Y | G | B | R | O deriving (Show, Eq, Enum, Ord)
data Face = Up | Down | Front | Back | Right | Left deriving (Show)
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

cubieColor :: CubieEmbed -> Face -> Maybe Color
cubieColor CubieEmbed { cubie=Edge c _, location=[_, 0, _] } Up = Just c
cubieColor CubieEmbed { cubie=Corner c _ _, location=[_, 0, _] } Up = Just c
cubieColor _ Up = Nothing
cubieColor CubieEmbed { cubie=Edge c _, location=[_, 2, _] } Down = Just c
cubieColor CubieEmbed { cubie=Corner c _ _, location=[_, 2, _] } Down = Just c
cubieColor _ Down = Nothing
cubieColor CubieEmbed { cubie=Edge _ c, location=[1, _, 0] } Front = Just c
cubieColor CubieEmbed { cubie=Edge c _, location=[_, 1, 0] } Front = Just c
cubieColor CubieEmbed { cubie=Corner _ c d, location=[x, y, 0] } Front =
  if x == y then Just c else Just d
cubieColor _ Front = Nothing
cubieColor CubieEmbed { cubie=Edge _ c, location=[1, _, 2] } Back = Just c
cubieColor CubieEmbed { cubie=Edge c _, location=[_, 1, 2] } Back = Just c
cubieColor CubieEmbed { cubie=Corner _ c d, location=[x, y, 2] } Back =
  if x == y then Just d else Just c
cubieColor _ Back = Nothing
cubieColor CubieEmbed { cubie=Edge _ c, location=[2, _, _] } Right = Just c
cubieColor CubieEmbed { cubie=Corner _ c d, location=[2, y, z] } Right =
  if y == z then Just c else Just d
cubieColor _ Right = Nothing
cubieColor CubieEmbed { cubie=Edge _ c, location=[0, _, _] } Left = Just c
cubieColor CubieEmbed { cubie=Corner _ c d, location=[0, y, z] } Left =
  if y == z then Just d else Just c
cubieColor _ Left = Nothing

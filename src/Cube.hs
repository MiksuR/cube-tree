module Cube where

import Prelude hiding (Right, Left)
import Data.Array.IArray
import Data.Maybe (fromJust)
import Data.Set (Set, fromList)

import Text.Format

data Color = W | Y | G | B | R | O deriving (Show, Eq, Enum, Ord)
data Face = Up | Down | Front | Back | Right | Left deriving (Show)
data Cubie = Center !Color | Edge !Color !Color |
             Corner !Color !Color !Color | Inside deriving (Show, Eq, Ord)
data CubieEmbed = CubieEmbed { cubie :: !Cubie,
                               location :: !(Int, Int, Int) } deriving (Show)
type Layer = [CubieEmbed]
type Cube = Array (Int, Int, Int) Cubie

rotate :: Cubie -> Cubie
rotate (Edge a b) = Edge b a
rotate (Corner a b c) = Corner c a b
rotate x = x

corners :: Set Cubie
corners = fromList [Corner W G O, Corner W O B, Corner W B R, Corner W R G,
                    Corner Y O G, Corner Y B O, Corner Y R B, Corner Y G R]
edges :: Set Cubie
edges = fromList [Edge W G, Edge W O, Edge W B, Edge W R,
                  Edge G O, Edge B O, Edge B R, Edge G R,
                  Edge Y G, Edge Y O, Edge Y B, Edge Y R]

getFace :: Cube -> Face -> Array (Int, Int) Color
getFace c f = array ((0, 0), (2, 2))
                    [((i, j), fromJust $ cubieColor (embedding i j f) f)
                     | i <- [0..2], j <- [0..2]]
  where
    embedding i j Up = CubieEmbed {cubie=c!(i,0,2-j), location=(i,0,2-j)}
    embedding i j Down = CubieEmbed {cubie=c!(i,2,j), location=(i,2,j)}
    embedding i j Front = CubieEmbed {cubie=c!(i,j,0), location=(i,j,0)}
    embedding i j Back = CubieEmbed {cubie=c!(2-i,j,2), location=(2-i,j,2)}
    embedding i j Right = CubieEmbed {cubie=c!(2,j,i), location=(2,j,i)}
    embedding i j Left = CubieEmbed {cubie=c!(0,j,2-i), location=(0,j,2-i)}


cubeNet :: Cube -> String
cubeNet c = format emptyNet $ map show colors
  where
    colors = [getFace c Up ! (i, j) | j <- [0..2], i <-[0..2]]
      ++ [getFace c Left ! (i, j) | j <- [0..2], i <-[0..2]]
      ++ [getFace c Front ! (i, j) | j <- [0..2], i <-[0..2]]
      ++ [getFace c Right ! (i, j) | j <- [0..2], i <-[0..2]]
      ++ [getFace c Back ! (i, j) | j <- [0..2], i <-[0..2]]
      ++ [getFace c Down ! (i, j) | j <- [0..2], i <-[0..2]]

cubieColor :: CubieEmbed -> Face -> Maybe Color
cubieColor CubieEmbed { cubie=Center c, location=(1, 0, 1) } Up = Just c
cubieColor CubieEmbed { cubie=Edge c _, location=(_, 0, _) } Up = Just c
cubieColor CubieEmbed { cubie=Corner c _ _, location=(_, 0, _) } Up = Just c
cubieColor _ Up = Nothing
cubieColor CubieEmbed { cubie=Center c, location=(1, 2, 1) } Down = Just c
cubieColor CubieEmbed { cubie=Edge c _, location=(_, 2, _) } Down = Just c
cubieColor CubieEmbed { cubie=Corner c _ _, location=(_, 2, _) } Down = Just c
cubieColor _ Down = Nothing
cubieColor CubieEmbed { cubie=Center c, location=(1, 1, 0) } Front = Just c
cubieColor CubieEmbed { cubie=Edge _ c, location=(1, _, 0) } Front = Just c
cubieColor CubieEmbed { cubie=Edge c _, location=(_, 1, 0) } Front = Just c
cubieColor CubieEmbed { cubie=Corner _ c d, location=(x, y, 0) } Front =
  if x == y then Just c else Just d
cubieColor _ Front = Nothing
cubieColor CubieEmbed { cubie=Center c, location=(1, 1, 2) } Back = Just c
cubieColor CubieEmbed { cubie=Edge _ c, location=(1, _, 2) } Back = Just c
cubieColor CubieEmbed { cubie=Edge c _, location=(_, 1, 2) } Back = Just c
cubieColor CubieEmbed { cubie=Corner _ c d, location=(x, y, 2) } Back =
  if x == y then Just d else Just c
cubieColor _ Back = Nothing
cubieColor CubieEmbed { cubie=Center c, location=(2, 1, 1) } Right = Just c
cubieColor CubieEmbed { cubie=Edge _ c, location=(2, _, _) } Right = Just c
cubieColor CubieEmbed { cubie=Corner _ c d, location=(2, y, z) } Right =
  if y == z then Just c else Just d
cubieColor _ Right = Nothing
cubieColor CubieEmbed { cubie=Center c, location=(0, 1, 1) } Left = Just c
cubieColor CubieEmbed { cubie=Edge _ c, location=(0, _, _) } Left = Just c
cubieColor CubieEmbed { cubie=Corner _ c d, location=(0, y, z) } Left =
  if y == z then Just d else Just c
cubieColor _ Left = Nothing

emptyNet :: String
emptyNet = "\
\       +-+-+-+\n\
\       |{0}|{1}|{2}|\n\
\       +-+-+-+\n\
\       |{3}|{4}|{5}|\n\
\       +-+-+-+\n\
\       |{6}|{7}|{8}|\n\
\       +-+-+-+\n\
\+-+-+-++-+-+-++-+-+-++-+-+-+\n\
\|{9}|{10}|{11}||{18}|{19}|{20}||{27}|{28}|{29}||{36}|{37}|{38}|\n\
\+-+-+-++-+-+-++-+-+-++-+-+-+\n\
\|{12}|{13}|{14}||{21}|{22}|{23}||{30}|{31}|{32}||{39}|{40}|{41}|\n\
\+-+-+-++-+-+-++-+-+-++-+-+-+\n\
\|{15}|{16}|{17}||{24}|{25}|{26}||{33}|{34}|{35}||{42}|{43}|{44}|\n\
\+-+-+-++-+-+-++-+-+-++-+-+-+\n\
\       +-+-+-+\n\
\       |{45}|{46}|{47}|\n\
\       +-+-+-+\n\
\       |{48}|{49}|{50}|\n\
\       +-+-+-+\n\
\       |{51}|{52}|{53}|\n\
\       +-+-+-+"

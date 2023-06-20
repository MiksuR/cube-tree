{-|
Module      : Cube
Description : Primitive definitions and auxiliary functions
Copyright   : (c) Miksu Rankaviita, 2023
License     : BSD-3-Clause license
Stability   : stable

The 3×3 Rubik's cube is defined as an array of cubies,
which have defined sticker colours.
This module contains a function for generating a net of a
given cube, which can be printed in the terminal.
-}
module Cube where

import Prelude hiding (Right, Left)
import Control.Arrow ((&&&))
import Data.Array.IArray
import Data.Maybe (fromJust)
import Data.Set (Set, fromList)

import Text.Format

-- |Possible colours of stickers indicated by a letter.
data Color = W -- ^ White
           | Y -- ^ Yellow
           | G -- ^ Green
           | B -- ^ Blue
           | R -- ^ Red
           | O -- ^ Orange
           deriving (Show, Eq, Enum, Ord)
data Face = Up | Down | Front | Back | Right | Left deriving (Show)
data Cubie = Center !Color | Edge !Color !Color |
             Corner !Color !Color !Color | Inside deriving (Show, Eq, Ord)
-- |This type allows to define a cubie outside the context of an existing cube.
data CubieEmbed = CubieEmbed { cubie :: !Cubie,
                               -- |The index, where the cubie is to be 'embedded' in a `Cube` object
                               location :: !(Int, Int, Int)
                             } deriving (Show)
-- |`Layer`s are used in the `ConstructForest` module to build `Cube`s layer-by-layer
type Layer = [CubieEmbed]
{-|
  Assuming the standard color scheme and orientation, where the white face is on top
  and green is in the front, the origin is at the front-up-left corner.
  The axes are oriented as follows:

  ```
  x:  Right
  y:  Down
  z:  Towards the back
  ```

  The orientation of the cubie is determined by the order of the colours in the type.
  See `corners` and `edges` for the standard orientations.
-}
type Cube = Array (Int, Int, Int) Cubie

cubiesToCube :: [CubieEmbed] -> Cube
cubiesToCube cs = array ((0,0,0),(2,2,2)) $ fixedCubies
                                         ++ [ (l, c)
                                            | CubieEmbed { cubie=c, location=l } <- cs ]
  where
    fixedCubies = [((1,1,1),Inside), ((1,1,0),Center G), ((1,1,2),Center B), ((2,1,1),Center R),
                  ((0,1,1),Center O), ((1,0,1),Center W), ((1,2,1), Center Y)]

{-|
  Returns the colour of the center cubie of the given face:
  +----+------+-------+------+-------+------+
  | Up | Down | Front | Back | Right | Left |
  +----+------+-------+------+-------+------+
  | W  |  Y   |  G    |   B  |   R   |   O  |
  +----+------+-------+------+-------+------+
-}
faceToColor :: Face -> Color
faceToColor Up = W
faceToColor Down = Y
faceToColor Front = G
faceToColor Back = B
faceToColor Right = R
faceToColor Left = O

{-|
  The set of all possible corners in the standard orientation:

  {Corner W G O, Corner W O B, Corner W B R, Corner W R G,
   Corner Y O G, Corner Y B O, Corner Y R B, Corner Y G R}
-}
corners :: Set Cubie
corners = fromList [Corner W G O, Corner W O B, Corner W B R, Corner W R G,
                    Corner Y O G, Corner Y B O, Corner Y R B, Corner Y G R]
{-|
  The set of all possible edges in the standard orientation:

  {Edge W G, Edge W O, Edge W B, Edge W R,
   Edge G O, Edge B O, Edge B R, Edge G R,
   Edge Y G, Edge Y O, Edge Y B, Edge Y R}
-}
edges :: Set Cubie
edges = fromList [Edge W G, Edge W O, Edge W B, Edge W R,
                  Edge G O, Edge B O, Edge B R, Edge G R,
                  Edge Y G, Edge Y O, Edge Y B, Edge Y R]

isCorner :: Cubie -> Bool
isCorner (Corner _ _ _) = True
isCorner _              = False

isEdge :: Cubie -> Bool
isEdge (Edge _ _) = True
isEdge _          = False

rotate :: Cubie -> Cubie
rotate (Edge a b) = Edge b a
rotate (Corner a b c) = Corner c a b
rotate x = x

-- |Returns the cubie in standard orientation
orient :: Cubie -> Cubie
orient = uncurry ($) . ((rot . rotNum) &&& id)
  where
    rot = flip $ (!!) . iterate rotate

-- |Return the number of time the cubie must be rotated to be oriented
rotNum :: Cubie -> Int
rotNum (Edge   W _  ) = 0
rotNum (Edge   Y _  ) = 0
rotNum (Edge   _ W  ) = 1
rotNum (Edge   _ Y  ) = 1
rotNum (Edge   G _  ) = 0
rotNum (Edge   B _  ) = 0
rotNum (Edge   _ _  ) = 1
rotNum (Corner W _ _) = 0
rotNum (Corner Y _ _) = 0
rotNum (Corner _ _ W) = 1
rotNum (Corner _ _ Y) = 1
rotNum (Corner _ _ _) = 2
rotNum _              = 0

-- |Checks if there is a face, where the two given cubies have the same colour.
shareColor :: CubieEmbed -> CubieEmbed -> Bool
shareColor e c = or [ cubieColor e f == cubieColor c f
                    | f <- normalToFace <$> normals e ]

-- |Compute the normal vectors that point in the direction
-- the stickers of a given cubie are facing.
normals :: CubieEmbed -> [(Int, Int, Int)]
normals CubieEmbed { cubie=_, location=(x,y,z) } =
  filter (/= (0,0,0)) [ (f x,0,0), (0,f y,0), (0,0,f z) ]
  where
    f 0 = -1
    f 2 =  1
    f _ =  0

normalToFace :: (Int, Int, Int) -> Face
normalToFace (1,0,0) = Right
normalToFace (0,1,0) = Down
normalToFace (0,0,1) = Back
normalToFace (-1,0,0) = Left
normalToFace (0,-1,0) = Up
normalToFace (0,0,-1) = Front
normalToFace _ = error "Not a valid normal vector!"

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

cubeNet :: Cube -> String
cubeNet c = format emptyNet $ map show colors
  where
    colors = [getFace c Up ! (i, j)    | j <- [0..2], i <-[0..2]]
          ++ [getFace c Left ! (i, j)  | j <- [0..2], i <-[0..2]]
          ++ [getFace c Front ! (i, j) | j <- [0..2], i <-[0..2]]
          ++ [getFace c Right ! (i, j) | j <- [0..2], i <-[0..2]]
          ++ [getFace c Back ! (i, j)  | j <- [0..2], i <-[0..2]]
          ++ [getFace c Down ! (i, j)  | j <- [0..2], i <-[0..2]]

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

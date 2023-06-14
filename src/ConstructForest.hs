{-|
Module      : ConstructForest
Description : Constructs forest of cube permutations
Copyright   : (c) Miksu Rankaviita, 2023
License     : BSD-3-Clause license
Stability   : stable

The `cubeForest` function constructs a forest in the following manner:

  1. Start with a list of corners all embedded at the origin.
  2. Let each corner be a root of a tree.
  3. For a given corner, pick three edges and embed them in the three spots adjacent to the origin
     in such a way that no two adjacent stickers have the same colour.
  4. Take such a triplet of edges to be a leaf of the corresponding tree.
  5. Continue this contstruction iteratively: For each leaf, construct a leaf by choosing a set of adjacent
     pieces, in such a way that no two adjacent stickers have the same colour.

The leaves in such forest define all the cubes with the property
that no two adjacent stickers have the same colour.
-}
module ConstructForest where

import Cube

import Control.Monad (join)
import Data.List (permutations)
import Data.Tree
import Data.Set (Set, fromList, toList, delete, foldl', difference)
import Data.Sequence (Seq(..), (><), (<|))

--   LayerSeed = (layer, edges, corners, layer number)
type LayerSeed = (Layer     -- ^ layer
                , Set Cubie -- ^ edges
                , Set Cubie -- ^ corners
                , Int       -- ^ layer number
                 )

cubeForest :: Forest Layer
cubeForest = unfoldForest buildNode firstCorners

firstCorners :: [LayerSeed]
firstCorners = [ (embedFirst (c, r), edges, delete c corners, 0)
               | c <- toList corners, r <- [id, rotate, rotate . rotate] ]
  where
    embedFirst (c, r) = [CubieEmbed { cubie = r c,  location = (0, 0, 0) }]

buildNode :: LayerSeed -> (Layer, [LayerSeed])
buildNode (layer, edgs, corns, n) = (layer, filter (valid layer) embedded)
  where
    embedded = embedLayers <$> combinations isCorners layerSize
                            $ if isCorners then corns else edgs
    isCorners = even n
    layerSize = if isCorners then 4-abs(n-3) else 6-abs((3*n-9) `div` 2)
    embedLayers :: Seq [Cubie] -> [LayerSeed]
    embedLayers = foldr ((:) . embedLayer) []
    embedLayer cs = ([ CubieEmbed { cubie = c, location = cubieLocation n i }
                     | (c, i) <- zip cs [0..] ],
                     if even n then edgs
                     else difference edgs (fromList cs),
                     if even n then difference corns (fromList cs)
                     else corns, n+1)

-- |Given a set of cubies, produce all possible rotations and permutations.
combinations :: Bool        -- ^ set of corners (True) or edges (False)
             -> Int         -- ^ layer size
             -> Set Cubie   -- ^ set of cubies
             -> Seq [Cubie] -- ^ combinations
combinations _ 0 _ = [] <| Empty
combinations isCorners n s = foldl' adjoiner Empty s
  where
    adjoiner l a = l >< (join $ applyRotations a
                             <$> combinations isCorners (n-1) (delete a s))
    applyRotations a combins = (\r -> r a:combins) <$> rotations
    rotations = id <| rotate <|
                (if isCorners then rotate . rotate <| Empty else Empty)

-- |Calculate the location of a cubie given the layer number and its position in the layer.
cubieLocation :: Int             -- ^ layer number
              -> Int             -- ^ cubie index
              -> (Int, Int, Int) -- ^ coordinates
cubieLocation 3 i = toTuple (permutations [0, 1, 2] !! i)
  where
    toTuple [a, b, c] = (a, b, c)
cubieLocation n i = (!! i) $ map transform [(1,0,0), (0,1,0), (0,0,1)]
  where
    transform (a, b, c) = if n < 3 then (n*a, n*b, n*c)
                          else (2-(6-n)*a, 2-(6-n)*b, 2-(6-n)*c)

-- |Test whether or not the cubies in the new layer share colours with those on the old layer.
valid :: Layer -> LayerSeed -> Bool
valid ol (nl, _, _, _) = and [ checkColors c | c <- nl ]
  where
    checkColors CubieEmbed {cubie = c, location = (x,y,z)} = undefined

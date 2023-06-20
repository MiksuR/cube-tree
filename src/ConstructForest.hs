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
import Data.Maybe (fromJust)
import Data.Tree
import Data.Set (Set, fromList, toList, delete, foldl', difference, empty)
import Data.Sequence (Seq(..), (><), (<|))

type LayerSeed = (Layer     -- ^ layer
                , Set Cubie -- ^ edges
                , Set Cubie -- ^ corners
                , Int       -- ^ layer number
                 )

cubeForest :: Forest Layer
cubeForest = unfoldForest buildNode firstCorners

cubeMiniTree :: Tree Layer
cubeMiniTree = unfoldTree buildMiniNode ([CubieEmbed {cubie=Corner W G O,
                                                      location=(0,0,0)}],
                                          empty, delete (Corner W G O) corners, 0)

firstCorners :: [LayerSeed]
firstCorners = [ (embedFirst (c, r), edges, delete c corners, 0)
               | c <- toList corners, r <- [id, rotate, rotate . rotate] ]
  where
    embedFirst (c, r) = [CubieEmbed { cubie = r c,  location = (0, 0, 0) }]

buildNode :: LayerSeed -> (Layer, [LayerSeed])
buildNode ([], _, _, _)           = ([],    [])
buildNode (layer, edgs, corns, n) = (layer, filter (valid layer) embedded)
  where
    embedded = embedLayers $ combinations isCorners layerSize
                           $ if isCorners then corns else edgs
    isCorners = odd n
    layerSize = if isCorners then 4-abs(n-2) else 6-abs((3*n-6) `div` 2)
    embedLayers :: Seq [Cubie] -> [LayerSeed]
    embedLayers = foldr ((:) . embedLayer) []
    embedLayer :: [Cubie] -> LayerSeed
    embedLayer cs = ([ CubieEmbed { cubie = c, location = cubieLocation (n+1) i }
                     | (c, i) <- zip cs [0..] ],
                     if isCorners then edgs
                     else difference edgs (fromList . map orient $ cs),
                     if isCorners
                     then difference
                            corns (fromList . map orient $ cs)
                     else corns, n+1)

buildMiniNode :: LayerSeed -> (Layer, [LayerSeed])
buildMiniNode ([], _, _, _)        = ([],    [])
buildMiniNode (layer, _, corns, n) = (layer, filter (validMini layer) embedded)
  where
    embedded = embedLayers $ combinations True layerSize corns
    layerSize = case n of 0 -> 3; 2 -> 3; 4 -> 1; _ -> 0
    embedLayers :: Seq [Cubie] -> [LayerSeed]
    embedLayers = foldr ((:) . embedLayer) []
    embedLayer :: [Cubie] -> LayerSeed
    embedLayer cs = ([ CubieEmbed { cubie = c, location = cubieLocation (n+2) i }
                     | (c, i) <- zip cs [0..] ], empty,
                     difference corns (fromList . map orient $ cs), n+2)

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
cubieLocation 3 i = [(2,1,0),(2,0,1),(0,2,1),(1,2,0),(1,0,2),(0,1,2)] !! i
cubieLocation n i = (!! i) $ map transform [(1,0,0), (0,1,0), (0,0,1)]
  where
    transform (a, b, c) = if n < 3 then (n*a, n*b, n*c)
                          else (2-(6-n)*a, 2-(6-n)*b, 2-(6-n)*c)

-- |Test whether or not the cubies in the new layer share colours with those on the old layer.
valid :: Layer -> LayerSeed -> Bool
valid (o:_) (nl, _, _, 1) = and $ [ not $ shareColor n o | n <- nl ]
                               ++ concat [ [ fromJust (cubieColor n f) /= faceToColor f
                                           | f <- normalToFace <$> normals n ]
                                         | n <- nl ]
valid ol (nl, _, _, 2) = and [ not $ shareColor (ol !! i) (nl !! i) | i <- [0..2] ]
valid ol (nl, _, _, 3) = and $ [ not $ shareColor (nl !! (2*i))   (ol !! i) | i <- [0..2] ]
                            ++ [ not $ shareColor (nl !! (2*i+1)) (ol !! i) | i <- [0..2] ]
                            ++ concat [ [ fromJust (cubieColor n f) /= faceToColor f
                                        | f <- normalToFace <$> normals n ]
                                      | n <- nl ]
valid ol (nl, _, _, 4) = and $ [ not $ shareColor (ol !! (2-i))   (nl !! i) | i <- [0..2] ]
                            ++ [ not $ shareColor (ol !! (5-i)) (nl !! i) | i <- [0..2] ]
valid ol (nl, _, _, 5) = and $ [ not $ shareColor (nl !! i) (ol !! i) | i <- [0..2] ]
                            ++ concat [ [ fromJust (cubieColor n f) /= faceToColor f
                                        | f <- normalToFace <$> normals n ]
                                      | n <- nl ]
valid ol (n:_, _, _, 6) = and [ not $ shareColor n o | o <- ol ]
valid _ ([],_,_,n) = n > 6
valid _ _ = error "Invalid layer seed!"

validMini :: Layer -> LayerSeed -> Bool
validMini (o:_) (nl,  _, _, 2) = and   [ not $ shareColor o n | n <- nl ]
validMini ol    (nl,  _, _, 4) = and $ [ not $ shareColor (ol !! i) (nl !! mod (i+1) 2) | i <- [0..2] ]
                                    ++ [ not $ shareColor (ol !! i) (nl !! mod (i-1) 2) | i <- [0..2] ]
validMini ol    (n:_, _, _, 6) = and   [ not $ shareColor o n | o <- ol ]
validMini _     ([],  _, _, n) = n > 6
validMini _ _ = error "Invalid layer seed!"

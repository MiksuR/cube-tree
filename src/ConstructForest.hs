module ConstructForest where

import Cube

import Data.Tree
import Data.Set (Set, toList, delete)

cubeForest :: Forest (Layer)
cubeForest = unfoldForest buildNode firstCorners

firstCorners :: [(Layer, Set Cubie, Set Cubie, Int)]
firstCorners = [ (embedFirst (c, r), edges, delete c corners, 0)
               | c <- toList corners, r <- [id, rotate, rotate . rotate] ]
  where
    embedFirst (c, r) = [CubieEmbed { cubie = r c,  location = (0, 0, 0) }]

buildNode :: (Layer, Set Cubie, Set Cubie, Int) -> (Layer, [(Layer, Set Cubie, Set Cubie, Int)])
buildNode (l, edgs, corns, n) = (l, filter (valid l) permutations)
  where
    permutations | mod n 2 == 0 = undefined
                 | mod n 2 == 1 = undefined

valid :: Layer -> (Layer, Set Cubie, Set Cubie, Int) -> Bool
valid ol (nl, edgs, corns, n) = undefined

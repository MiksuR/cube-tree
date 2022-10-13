module ConstructForest where

import Cube

import Control.Monad (join)
import Data.Tree
import Data.Set (Set, fromList, toList, delete, foldl', difference)
import Data.Sequence (Seq(..), (><), (<|))

--   LayerSeed = (layer, edges, corners, layer number)
type LayerSeed = (Layer, Set Cubie, Set Cubie, Int)

cubeForest :: Forest (Layer)
cubeForest = unfoldForest buildNode firstCorners

firstCorners :: [LayerSeed]
firstCorners = [ (embedFirst (c, r), edges, delete c corners, 0)
               | c <- toList corners, r <- [id, rotate, rotate . rotate] ]
  where
    embedFirst (c, r) = [CubieEmbed { cubie = r c,  location = (0, 0, 0) }]

buildNode :: LayerSeed -> (Layer, [LayerSeed])
buildNode (layer, edgs, corns, n) = (layer, filter (valid layer) embedded)
  where
    embedded = embedN <$> combinations edgsOrCors layerSize
                       $ if edgsOrCors == 0 then corns else edgs
    edgsOrCors = mod n 2
    layerSize = if edgsOrCors == 0 then 4-abs(n-3) else 6-abs((3*n-9) `div` 2)
    embedN :: Seq [Cubie] -> [LayerSeed]
    embedN = undefined
    {--
    embedN cs = ([ CubieEmbed { cubie = c, location = cubieLocation n i }
                 | (c, i) <- zip cs [0..] ],
                 if mod n 2 == 0 then edgs else difference edgs (fromList cs),
                 if mod n 2 == 0 then difference corns (fromList cs) else corns,
                 n+1)
    --}

combinations :: Int -> Int -> Set Cubie -> Seq [Cubie]
combinations _ 0 _ = [] <| Empty
combinations edgsOrCors n s = foldl' adjoiner Empty s
  where
    adjoiner l a = l >< (join $ (applyRotations a)
                             <$> combinations edgsOrCors (n-1) (delete a s))
    applyRotations a combins = (\r -> (r a):combins) <$> rotations
    rotations = id <| rotate <|
                (if edgsOrCors == 0 then rotate . rotate <| Empty else Empty)

cubieLocation :: Int -> Int -> (Int, Int, Int)
cubieLocation n i = undefined

valid :: Layer -> LayerSeed -> Bool
valid ol (nl, _, _, _) = and [ checkColors c | c <- nl ]
  where
    checkColors (CubieEmbed {cubie = c, location = (x,y,z)}) = undefined

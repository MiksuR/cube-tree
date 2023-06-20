module Main (main) where

import ConstructForest
import Cube
import Solvability

import Data.Tree

accumToLeaf :: Tree a -> [Int] -> [a]
accumToLeaf _ [] = []
accumToLeaf (Node x []) _ = [x]
accumToLeaf t (i:is) = rootLabel subTree : accumToLeaf subTree is
  where
    subTree = subForest t !! i

paths :: Forest a -> [[Int]]
paths f = concat [ map (i:) $ foldTree pathBuilder t | (t, i) <- zip f [0..] ]
  where
    pathBuilder :: a -> [[[Int]]] -> [[Int]]
    pathBuilder _ [] = [[]]
    pathBuilder _ ps = concat [ map (i:) p | (p, i) <- zip ps [0..] ]

-- |List of 3x3 solvable cubes such that no two adjacent stickers
-- have the same colour.
cubes :: [Cube]
cubes = map cubiesToCube . filter (\x -> length x == 20 && solvable (map cubie x))
      . concatMap (foldTree (\x xs -> if null xs then [x]
                                      else map (x++) . concat $ xs))
      $ cubeForest

-- |List of 2x2 solvable cubes such that no two adjacent stickers
-- have the same colour.
--
-- Runnig `main = print $ length miniCubes` executes in ~12s
-- on my machine, and returns the value 326188.
miniCubes :: [[Cubie]]
miniCubes = filter (\x -> length x == 8 && solvableMini x) . map (map cubie)
          . foldTree (\x xs -> if null xs then [x]
                               else map (x++) . concat $ xs)
          $ cubeMiniTree

main :: IO ()
main = print $ length miniCubes

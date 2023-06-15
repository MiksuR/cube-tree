module Main (main) where

import ConstructForest
import Cube

import Data.Tree

accumToLeaf :: Tree a -> [Int] -> [a]
accumToLeaf _ [] = []
accumToLeaf (Node x []) _ = [x]
accumToLeaf t (i:is) = rootLabel subTree : accumToLeaf subTree is
  where
    subTree = subForest t !! i

main :: IO ()
main = putStrLn . cubeNet . cubiesToCube
                . (CubieEmbed {cubie = Corner W G O, location = (0,0,0)}:)
                . concat $ accumToLeaf (head cubeForest) [0,0,0,0,0,0]

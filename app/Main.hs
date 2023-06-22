module Main (main) where

import ConstructForest
import Cube
import Solvability

import Data.List.Extra ((!?))
import Data.Maybe (catMaybes)
import Data.Tree
import System.Random

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

-- Using this list to estimate the size of layer 2 gives the following value: 63881392.
meanChildCount :: [(Int, Int)]
meanChildCount = [ (br t, div (sum . map br $ subForest t) (br t)) | t <- cubeForest ]
  where
    br = length . subForest

randomCubeTree :: (RandomGen g) => Int -> g -> Maybe (Tree Layer, [CubieEmbed])
randomCubeTree level gen = run level newGen [] $ cubeForest !! treeInd
  where
    (treeInd, newGen) = randomR (0, 23) gen
    run :: (RandomGen g) => Int -> g -> [CubieEmbed] -> Tree Layer -> Maybe (Tree Layer, [CubieEmbed])
    run 0 _    acc t = Just (t, acc)
    run l gen' acc t = let (ind, newGen') = randomR (0, maxInd t $ level-l) gen'
                       in subForest t !? ind
                      >>= run (l-1) newGen' (acc ++ rootLabel t)
    maxInd t l = if l == 2 then 10000 else length (subForest t) - 1

retryUntilSuccess :: (RandomGen g) => (g -> Maybe a) -> g -> [a]
retryUntilSuccess f gen = catMaybes $ tries gen
  where tries g = let (g1, g2) = split g in f g1 : tries g2

-- This computation yields an average of 54.
main :: IO ()
main = do
  let samples = 10
  gen <- getStdGen
  let trees = take samples $ retryUntilSuccess (randomCubeTree 3) gen
  let toCubes (t, acc) = filter (\x -> length x == 20 && solvable (map cubie x))
                     . map (acc++) $ foldTree
                                     (\x xs -> if null xs then [x]
                                               else map (x++) . concat $ xs) t
  let cubeCounts = map (length . toCubes) trees
  print $ div (sum cubeCounts) samples

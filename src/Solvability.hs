{-|
Module      : Solvability
Description : Checks if a cube is solvable
Copyright   : (c) Miksu Rankaviita, 2023
License     : BSD-3-Clause license
Stability   : stable
-}

module Solvability where

import Cube

import Math.Combinat.Permutations

-- |Check if a cube can be solved.
--
-- This can be checked simply by computing the parities of
-- the permutation solving the cube, and is thus
-- not too inefficient.
--
-- The function assumes that the list is ordered correctly.
-- Below is the order in which the cubies are assumed to appear.
-- ```
-- (0,0,0), (1,0,0), (0,1,0), (0,0,1), (2,0,0), (0,2,0), (0,0,2),
-- (2,1,0), (2,0,1), (0,2,1), (1,2,0), (1,0,2), (0,1,2),
-- (0,2,2), (2,0,2), (2,2,0), (1,2,2), (2,1,2), (2,2,1), (2,2,2)
-- ```
solvable :: [Cubie] -> Bool
solvable c | length c == 20 = permutable c && orientable c
           | otherwise      = error $ show c ++ " does not define a cube!"

solvableMini :: [Cubie] -> Bool
solvableMini c | length c == 8 = (== 0) . (`mod` 3) . sum . map rotNum $ c
               | otherwise     = error $ show c ++ " does not define a cube!"

permutable :: [Cubie] -> Bool
permutable = isEvenPermutation . toPermutationUnsafeN 20 . map ((+1) . cubieIndex)

orientable :: [Cubie] -> Bool
orientable c = (even              . sum . map rotNum $ edgs)
            && ((==0) . (`mod` 3) . sum . map rotNum $ cors)
  where
    edgs = filter isEdge c
    cors = filter isCorner c

-- Returns the index of the `Cubie` in the solved position.
cubieIndex :: Cubie -> Int
cubieIndex (Corner W G O) = 0
cubieIndex (Edge   W G  ) = 1
cubieIndex (Edge   G O  ) = 2
cubieIndex (Edge   W O  ) = 3
cubieIndex (Corner W R G) = 4
cubieIndex (Corner Y O G) = 5
cubieIndex (Corner W O B) = 6
cubieIndex (Edge   G R  ) = 7
cubieIndex (Edge   W R  ) = 8
cubieIndex (Edge   Y O  ) = 9
cubieIndex (Edge   Y G  ) = 10
cubieIndex (Edge   W B  ) = 11
cubieIndex (Edge   B O  ) = 12
cubieIndex (Corner Y B O) = 13
cubieIndex (Corner W B R) = 14
cubieIndex (Corner Y G R) = 15
cubieIndex (Edge   Y B  ) = 16
cubieIndex (Edge   B R  ) = 17
cubieIndex (Edge   Y R  ) = 18
cubieIndex (Corner Y R B) = 19
cubieIndex e@(Edge _ _)   = cubieIndex $ orient e
cubieIndex c@(Corner _ _ _) = cubieIndex $ orient c
cubieIndex c              = error $ show c ++ " is neither an Edge nor a Corner!"

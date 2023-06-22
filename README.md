1. [Introduction](README.md#introduction)
2. [Implementation](README.md#implementation)
3. [Computing the answer](README.md#computing-the-answer)
   1. [The 2×2 case](README.md#the-22-case)
   2. [Estimation](README.md#estimation)
4. [Conclusion](README.md#conclusion)

# Introduction
When scrambling the 3×3 Rubik's cube, one might reach a configuration
of the cube, where no two adjacent stickers have the same colour.
However, based on experience this seems to be rather rare an occurrance.
Even if one tries to reach such a configuration by breaking up blocks of the same colour,
it is not easy to reach one.
An interesting question is, "How many such configurations are there?"
To count the number of such configurations, one should first understand
how the Rubik's cube works.

The cube consists of 26 pieces called "cubies" ($3^3=27$, 
but the inside of the cube is not a piece). There are three types of cubies:
centers, edges and corners.
  1. The center pieces have one sticker on them, 
     and they stay in their places when one turns the cube.
  2. The edge pieces have two stickers and there are 12 of them.
  3. The corners have three stickers and there are 8 of them.

Then, we can start with 12 edges and 8 corners and count the number of
ways to assemble them together to form a cube with the wanted property.

To be more precise, the procedure is as follows:
  1. Choose an arbitrary corner.
  2. Attach three edges to the corner is such a way that no two adjacent
     stickers have the same colour.
  3. Continue this procedure inductively by attaching new cubies to the
     existing cubies while preserving the property.

At each step of the procedure, we need to keep track of the cubies
that are already inserted and which we can still use, so that we do not
accidentally insert the same cubie twice.
After running out of pieces to insert, we have assembled a cube,
where no adjacent stickers have the same colour.

This process defines _an abstract tree structure_, where nodes are partially
assembled cubes, and the children of a given node are all the different
ways of attaching cubies to the partially assembled cube while preserving
the property. Then, our counting problem is the same as counting the number
of leaves of the tree at the lowest level, and subtracting off the number
of the leaves that define cubes that are not solvable.
I have written a Haskell program that defines this tree structure, which can
theoretically be used to count the number of the combinations.
A huge benefit to using Haskell is that we can use lazy evaluation to
define the tree object and explore parts of it without actualizing the
whole tree at once. Hence, we do not need to worry about memory limitations,
while still being able to write the code as if the entire tree existed in memeory.

# Implementation
To start with, I wrote useful type definitions for defining cubes and
auxiliary functions for manipulating the objects.
`Cubie` is the type encoding a piece of a Rubik's cube. It is defined by
its type (`Center`, `Edge`, `Corner`), the colours of its stickers
(white, yellow, green, blue, red, orange), and its orientation.
Then, `Cube` is an array of `Cubie`s, where there is precisely
one `Cubie` for each coordinate $(x,y,z)$ between $(0,0,0)$
and $(2,2,2)$. Since index $(1,1,1)$ of the array must also
be populated, I have added an `Inside` element to the type
`Cubie` even though it does not actually represent a cubie.

To construct the tree structure, I use the `containers` package,
which exports the `Data.Tree` module. One caveat in the actual
implementation is that I actually use a `Forest` type instead of
`Tree`. A forest is simply a set of trees:
```haskell
type Forest a = [Tree a]
```
There is one tree for
each choice of a corner, and these form a forest.
`Forest`s are usually constructed using the
`unfoldForest` function which has the following type signature.
```haskell
unfoldForest :: (b -> (a, [b])) -> [b] -> [Tree a]
```
The function applies the given function to a list of seed values,
which produces a node and a list of seeds, that will "grow out"
to be its children.

In my program, I define

``` haskell
cubeForest = unfoldForest buildNode firstCorners
```
The list `firstCorners` consists of seeds corresponding to the 24
corners, which are the 8 corners rotated in the 3 different ways.
From each such seed, the `buildNode` function constructs a list
of three edges and checks that if they are placed around the corner,
no two adjacent stickers have the same colour, and so on.
The seed values include the set of available cubies that can be used
to construct the next seed values. When there are no more available
cubies, `buildNode` returns the empty list, and the process terminates.

Next, this `Forest` object can be folded into a list of `Cube`s
using the `foldTree` function from the module `Data.Tree`.
But the resulting list contains cubes that cannot be solved, so the final
step is to filter those out. To do this, I wrote the `solvable`
function, which checks whether a given cube can be solved.
Such a function might sound complicated, but it is actually
quite simple. I use the group-theoretic fact that a cube is
solvable, if and only if it can be solved using an even permutation, and
simply check the parity of the permutation using the `Math.Combinat.Permutations`
module from the `combinat` package. Applying `filter solvable`
to the list of cubes obtained from the forest yields the
solvable cubes we want.

# Computing the answer
After finishing the implementation, I found out that the tree
structure is so massive that there is no way to compute the number
of leaves, at least with my laptop in a reasonable amount of time.
I wanted to still get some results out of the program, so I did two things:
First, I tweaked the code a little bit to construct the analogous tree
structure for the 2×2 Rubik's cube. Then, I went back to the 3×3
case and tried to compute some sort of an estimate for the number
of the cubes by sampling random branches from the forest.

## The 2×2 case
I added a few lines to the code to define the
`cubeMiniTree` tree, which is analogous to `cubeForest`.
In the 2×2 case we actually use a tree instead of a forest,
because the 2×2 cube does not have a fixed orientation,
hence we can simply fix the first corner to be some
chosen corner. Then, I defined the list `miniCubes`,
which contains all the solvable 2×2 cubes, where no
two adjacent stickers have the same colour.
Running `length miniCubes` returns the value of
`326,188`. Since there are `3,674,160` solvable configurations
of the 2×2 Rubik's cube, `8.9%` of them have the property
that no two adjacent stickers have the same colour.

## Estimation
The level 0 of `cubeForest` consists of the 24 corners.
First, I counted the number of children each of the
24 root nodes has. Summing these values yields the
size of level 1 of the forest, which is `32,142`. 
Next, for each corner on level 0, I counted 
the numbers of children of each child of the root node, 
and took their average. Then, I obtained an estimate
of the size of level 2 by summing together the products of
the number of children by the average number of "grand children".
For example, the first root node has `3,716` children. 
When I sum the number of children of each of the `3,716` child
and divide by `3,716`, I get `1987`. Then, the contribution
of the first root node to the size of level 2 is
`3,716 * 1,987 = 7,383,692`. The final estimate 
for the size of level 2 I obtained is `63,881,392`.

Level 3 is absolutely massive, so I was able to give only
a very rough estimate. What I ended up doing is taking random nodes
from level 2 and computing the number of their children.
The number of children varied from `20,000` to `120,000`, but on
average was around `65,000`. Therefore, I estimate the
size of level 3 to be `65,000 * 63,881,392 = 4,152,290,480,000`.

Finally, I took random nodes from level 3, and for each node,
I computed the number of all the leaves that define
solvable cubes. On average, there were `54` such cubes.
This was not too difficult to compute, since there weren't
too many of them. Hence, the final estimate is
`54 * 4,152,290,480,000 = 224,223,685,920,000`. There are 
`43,252,003,274,489,856,000` solvable combinations of the Rubik's cube
in total, and we have
$$\frac{224,223,685,920,000}{43,252,003,274,489,856,000}=5.184\times 10^{-6}$$
Therefore, the number is still excedingly small compared to the
total number of solvable cubes.

# Conclusion
What surprised me in this project was the sheer number of
combinations with the property. But there are still things to explore,
and my program can be used to further study the properties
of the forest.
I hope that someone with better understanding of statistics
and access to more computing power would be able to
refine the estimate.

In addition to the counting problem, I had another vague
question: "Are the cubes with this property more difficult
to solve than a random cube?" It would be interesting
to take random samples from the forest, compute
the number of moves needed to solve them, and compare the
data with the same numbers when sampled from the set
of all solvable cubes.

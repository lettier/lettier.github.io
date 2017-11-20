---
title: Breadth-First Search in Haskell
jumbotron_image: /images/2016-04-29-breadth-first-search-in-haskell/jumbotron_image.jpg
preview_image: /images/2016-04-29-breadth-first-search-in-haskell/preview_image.jpg
description: Using Haskell, we implement the breadth-first search algorithm.
author: David Lettier
---
<!--https://pixabay.com/static/uploads/photo/2015/01/30/18/26/spider-web-617769_960_720.jpg-->

The full source code to this post can be found on [GitHub](https://github.com/lettier/interviewquestions/blob/master/graphs/bfs.hs).

## Data Types

```haskell
data Vertex = Vertex {
                          vertexLabel :: [Char]
                        , vertexNeighbors :: [[Char]]
                        , vertexDistance :: Int
                        , vertexPredecessor :: [Char]
                      } deriving (Show)

-- We define a graph as a list of vertexes.
-- Each vertex is a label, an adjacency list (neighbors),
-- a distance away from the root, and a predecessor label.
data Graph = Graph [Vertex] deriving (Show)
```

We will begin by modeling a graph as a list of vertexes which themselves hold the edges in adjacency lists.

## Getters

```haskell
-- Takes in a vertex, a list of vertexes, and returns true or false.
vertexInVertexes :: Vertex -> [Vertex] -> Bool
-- Given an empty list of vertices, return false.
vertexInVertexes _ [] = False
-- Reduce the list of vertexes to a bool.
-- If at least one vertex label in the list matches the input vertex label, the result will be true.
vertexInVertexes Vertex {vertexLabel = label} (x:y) = foldl (\ acc x -> vertexLabel x == label || acc) False (x:y)

-----------------------------------------------------------------------------

-- Takes a graph, a list of strings, and outputs a list of vertexes.
graphVertexes :: Graph -> [[Char]]-> [Vertex]
-- Empty graph.
graphVertexes (Graph []) _ = []
graphVertexes (Graph (x:y)) [] = x : y
-- The vertex label is an element in the list of labels (keys).
graphVertexes (Graph (x:y)) keys = filter (\ z -> vertexLabel z `elem` keys) (x:y)
```

For convenience, we will define two functions for extracting what we need out of a vertex or a graph.

## Breadth-First Search (BFS)

[BFS](https://www.cs.usfca.edu/~galles/visualization/BFS.html) is a way to traverse or travel a graph and output a tree
(a spanning tree if the graph is connected).
The output tree being a sub-graph with a root vertex, no cycles, and includes every vertex
(reachable from the root) in the graph but not necessarily all of the graph’s edges.
This tree grows out from the start vertex or root.
Each vertex in each branch of the tree lists the distance back to the root (start vertex).
Applications for BFS include determining if there is a path from vertex A to B and finding
the shortest path from A to vertex B in unweighted graphs.

We start at a certain vertex (making it the root) and look at its edges.
For each edge, we write down the neighboring vertex.
Before we leave, we take each neighbor vertex and set its distance to one more than the current distance.
We also set the parent or predecessor vertex as the current vertex we are standing on.
Just before we are done with the current vertex, we need to add all of its neighbors
(minus any we added before) to a queue or waiting line.

```haskell
                   Queue
                   ______________________

  v1             < v2, v3                 < v4, v5, v6
  Current Vertex   ______________________   Neighbors
```

If we added vertexes we added before, we may never finish our travels since there might be a cycle in the graph.
With the neighbors added, we pluck the next vertex from the queue and do it all over again.
If the queue is empty, we are done and we output our BFS tree.

```haskell
  a --- b       a.0 --- b.1
  -     -       -       -
  -     -  >>>  -       -
  -     -       -       -
  c --- d       c.1     d.2
```

```haskell
--     In       Out      Queue       Seen        Out
bfs :: Graph -> Graph -> [Vertex] -> [Vertex] -> Graph
bfs (Graph []) _ _ _ = Graph []
-- If the queue is empty, output the breadth-first search tree (graph).
bfs _ outGraph [] _ = outGraph
bfs (Graph (a:b)) (Graph (c:d)) (e:f) (g:h) = bfs inGraph outGraph queue seen'
    where inGraph = Graph (a:b)
          -- Get the current vertex label.
          eLabel = vertexLabel e
          -- Get the list of labels that are the neighbors of the current vertex.
          eNeighbors = vertexNeighbors e
          -- Get the vertexes from the vertex neighbor labels.
          eVertexNeighbors = graphVertexes inGraph eNeighbors
          -- The current distance, for the current vertex neighbors, is one more then
          -- the distance the current vertex is at.
          dist = vertexDistance e + 1
          -- Seen is the vertexes that have been queued before.
          seen = g : h
          -- Remove all neighbors, to the current vertex, that have
          -- been queued up before.
          filteredNeighbors = filterVertexNeighbors seen eVertexNeighbors
          -- Update the predecessor label and distance for each current vertex neighbor.
          enqueue = updateDistPred filteredNeighbors dist eLabel
          -- Update our breadth-first search tree/graph.
          outGraph = Graph $ (c:d) ++ enqueue
          -- Add the neighbors to the queue.
          queue = f ++ enqueue
          -- Update seen with the enqueued vertexes.
          seen' = seen ++ enqueue
```

If the input graph is empty, just output an empty graph. When the queue is empty,
output the BFS tree we recursively built.
Otherwise, call the function again with the input graph, the BFS tree plus the <i>have-not-seen-yet</i> neighbors,
the queue minus the current vertex plus the neighbors we haven’t seen yet, and all of the seen or
<i>added-to-the-queue-already</i> vertexes.

## Helper Functions

```haskell
filterVertexNeighbors :: [Vertex] -> [Vertex] -> [Vertex]
-- If either `s` (seen) or `vn` (vertex neighbors) are empty, return an empty list.
filterVertexNeighbors _ [] = []
filterVertexNeighbors [] _ = []
-- Filter out all vertexes in `vn` that are also in `s`.
filterVertexNeighbors s vn = filter (\ x -> not $ vertexInVertexes x s) vn

-----------------------------------------------------------------------------

updateDistPred :: [Vertex] -> Int -> [Char] -> [Vertex]
updateDistPred [] _ _ = []
-- Go though each vertex and swap the current distance and predecessor labels with the new parameters.
updateDistPred (x:y) dist predLabel = map (\ (Vertex label n _ _) -> Vertex label n dist predLabel) (x:y)
```

We use these functions in the `bfs` function up above.

## Input

```haskell
-- The main entry point for the program.
main :: IO ()
main = do
  -- Create a graph of nine vertexes.
  let inGraph = Graph [
                  Vertex "a" ["b", "c"          ] 0 ""
                , Vertex "b" ["a", "d", "e"     ] 0 ""
                , Vertex "c" ["a", "d"          ] 0 ""
                , Vertex "d" ["b", "c", "e"     ] 0 ""
                , Vertex "e" ["b", "d", "f", "g"] 0 ""
                , Vertex "f" ["e", "g", "h"     ] 0 ""
                , Vertex "g" ["e", "f", "i"     ] 0 ""
                , Vertex "h" ["f", "i"          ] 0 ""
                , Vertex "i" ["g", "h"          ] 0 ""
                ]
  -- Start the queue off with the starting vertex/root.
  let queue = graphVertexes inGraph ["e"]
  let outGraph = Graph queue
  let seen = queue
  printGraph $ bfs inGraph outGraph queue seen
  return ()
```

We will generate some static input consisting of a graph with nine vertexes.
You can see the graph drawn below. Before we can kick off `bfs`, we start the queue,
the BFS tree, and the seen list off with the root vertex (in this case `e`).

## Output

```haskell
-- An impure function that takes a graph and performs input/output (IO).
printGraph :: Graph -> IO ()
printGraph (Graph []) = putStrLn ""
printGraph (Graph (x:y)) = do
  -- Print the first vertex.
  print x
  -- Print the rest of the vertexes.
  printGraph (Graph y)
  return ()
```

To better visualize the BFS tree, we define a function that pretty prints the vertexes of a graph.

```haskell
a --- b -   - f --- h
-     -  - -  -     -
-     -   e   -     -
-     -  - -  -     -
c --- d -   - g --- i

/trees git:master ❯❯❯ runhaskell bfs.hs
Vertex {vertexLabel = "e", vertexNeighbors = ["b","d","f","g"], vertexDistance = 0, vertexPredecessor = ""}
Vertex {vertexLabel = "b", vertexNeighbors = ["a","d","e"], vertexDistance = 1, vertexPredecessor = "e"}
Vertex {vertexLabel = "d", vertexNeighbors = ["b","c","e"], vertexDistance = 1, vertexPredecessor = "e"}
Vertex {vertexLabel = "f", vertexNeighbors = ["e","g","h"], vertexDistance = 1, vertexPredecessor = "e"}
Vertex {vertexLabel = "g", vertexNeighbors = ["e","f","i"], vertexDistance = 1, vertexPredecessor = "e"}
Vertex {vertexLabel = "a", vertexNeighbors = ["b","c"], vertexDistance = 2, vertexPredecessor = "b"}
Vertex {vertexLabel = "c", vertexNeighbors = ["a","d"], vertexDistance = 2, vertexPredecessor = "d"}
Vertex {vertexLabel = "h", vertexNeighbors = ["f","i"], vertexDistance = 2, vertexPredecessor = "f"}
Vertex {vertexLabel = "i", vertexNeighbors = ["g","h"], vertexDistance = 2, vertexPredecessor = "g"}

a.2 --- b.1 -   - f.1 --- h.2
              - -
              e.0
              - -
c.2 --- d.1 -   - g.1 --- i.2
```

## Recap

Using Haskell, we defined a graph as a list of vertexes and a vertex as a data structure consisting of a label,
an adjacency list, a distance to the root, and a parent vertex label.
Our BFS function traversed our input graph recursively and output a BFS tree.
For each vertex in the output tree, it specifies how far away from the root it is and its parent vertex.

module HeuristicSearch where

import PriorityQueue
import qualified Data.Map as M

type Vertex = Int -- Actually depends on the application, let's go with this though
type Cost = Integer -- Cost is always positive

-- Graphs are represented as functions from a vertex to a list of tuples containing adjacent vertices and their corresponding cost
type Graph = Vertex -> [(Vertex, Cost)]

-- Heuristics are a function that given a Vertex, produce a Cost.
type Heuristic = Vertex -> Cost

-- Paths are constructed in reverse order, so the endpoint of the path is the first element in the list of vertices.
type Path = ([Vertex], Cost)

end :: Path -> Vertex
end = head . fst

cost :: Path -> Cost
cost = snd

extract :: Path -> Path
extract (vs, c) = (reverse vs, c)


-- Definition of the T* algorithm
tstar :: Graph -> Heuristic -> (Vertex -> Bool) -> Vertex -> Maybe Path
tstar g h goal source = tsearch start
    where start = insertQ ([source], 0) (h source) emptyQ;
            tsearch ps | nullQ ps = Nothing
                       | goal (end p) = Just (extract p)
                       | otherwise = tsearch rs
                            where (p, qs) = popQ ps; rs = addListQ (succs g h p) qs

-- As inputs to tstar we have a graph, a heuristic function, a test for whether a vertex
-- is a goal or not, and the source vertex. The frontier is maintained as a priority queue
-- of paths and their costs, initially containing the single path [source] with cost 0 and
-- priority h(source). If the queue is not empty, then a path with the lowest estimate of
-- how much it costs to complete the journey is selected. If the selected path ends at a
-- goal node, then that path is the result; otherwise its successor paths are added to the
-- queue.

-- succs returns a list of possible successor paths
succs :: Graph -> Heuristic -> Path -> [(Path, Cost)]
succs g h (u:vs, c) = [((v:u:vs, c+d), c + d + h v) | (v, d) <- g u]

-- The priority of the new path is not just an estimate of how far away the endpoint is from a goal, but the sum
-- of the cost of getting to the endpoint and the estimate of the remaining cost.


-- However, tstar is not guaranteed to terminate. 

-- A <--1--> B        C
-- Will oscillate between A and B and never end.

-- Also it can be very inefficient on some graphs
-- A <--1--> B <-100-> C
-- Will oscillate 100 times between A and B before finding the shortest path (this can be even worse).

-- Ok, but how do we fix this?

-- Maintain a finite map from vertices to path costs. If a new path to a vertex is discovered with a lower cost, then we can reexplore that path further (we are still only assuming that our heuristic is only admissible, not consistent).

-- Instead of making our own map, lets use Data.Map. (we imported this at the top of the page)

-- First, define `better` which determines (given a path) whether a better one has already been found
-- So works more like the opposite of what you'd think: If this is true then you DON'T want to add to the map
better :: Path -> M.Map Vertex Cost -> Bool
better (v:vs, c) vcmap = query (M.lookup v vcmap)
                        where query Nothing = False;
                              query (Just c') = c'<=c

-- Also define `add` which adds a new vertex-cost pair, or overwrites an old binding with the same vertex and the new cost
add :: Path -> M.Map Vertex Cost -> M.Map Vertex Cost
add (v:vs, c) vcmap = M.insert v c vcmap

-- And now we can define astar:
astar :: Graph -> Heuristic -> (Vertex -> Bool) -> Vertex -> Maybe Path
astar g h goal source = asearch M.empty start
    where start = insertQ ([source], 0) (h source) emptyQ;
            asearch vcmap ps | nullQ ps = Nothing -- vcmap -> vector-cost map
                             | goal (end p) =  Just (extract p)
                             | better p vcmap = asearch vcmap qs
                             | otherwise = asearch (add p vcmap) rs
                                    where (p, qs) = popQ ps; rs = addListQ (succs g h p) qs


-- How do we know this terminates for all inputs?

-- There are a finite number of non-vertex repeating paths in any finite graph.
-- Because the edge weights are positive numbers, no path with repeated vertices can have a smaller cost
-- than the corresponding path without repeated vertices that has the same destination.

-- Say $M$ is the maximum cost over all non-vertex repeating paths.
-- Then, any vertex can be processed a maximum of $M$ times, since each processing step requires a 
-- strictly smaller cost than before.
-- So it must be the case that `astar` terminates after AT MOST $M\times n$ steps, where $n$ is the number of nodes in the graph.
-- If no path exists from the source to the goal, then the function correctly returns `Nothing`.


-- How do we show that `astar` returns the optimal solution?

-- First, assume that there is a path from the source to the goal (we've already shown that astar can identify when such a path does not exist).
-- Say a path $p$ with endpoint $v$ is open if $p$ is on the frontier and there is no entry $(v,c)$ in the finite map with 
-- $c \leq C(p)$. Otherwise, say $p$ is closed. Open paths are candidates for further exploration, closed ones are not.

-- Let $P = [v_0, v_1, \dots, v_n]$ be the optimal path from the source $v_0$ to the goal $v_n$. Let $P_j = [v_0, \dots, v_j]$ denote the inital $j$ steps of that path, for $0 \leq j < n$.
-- We know at each step there is an open path $p$ with endpoint $v_j$ for some $j$ such that $c(p) = c(P_j)$
-- TODO: Do we? Probably a proof by induction. $p$ must contain $v_0$, that's the base case... LMAO see below I can't read
-- Hence, $p$ can be completed to an optimal path. 

-- The assertion holds at the base case because $P_0$ is open. Otherwise, let $D$ be the set of vertices $v_i$ for which there is a closed path $q$ from $v_0$ to $v_i$ on the frontier with $c(q) = c(P_i)$. The set $D$ is not empty since it contains $v_0$. 
-- Let $v_i$ be the vertex with the largest index in $D$ and set $j=i+1$. Define $p$ to be the path $q$ followed by the single edge $(v_i, v_j)$ with cost $c$. Then, $p$ is an open path and 
-- $$c(p) = c(q) + c = c(P_i) + c = c(P_j)$$
-- Thus, `astar` returns an optimal solution.










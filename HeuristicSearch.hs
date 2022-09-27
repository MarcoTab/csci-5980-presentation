module HeuristicSearch where

import PriorityQueue

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










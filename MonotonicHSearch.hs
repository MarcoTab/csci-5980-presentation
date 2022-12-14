-- Lets assume something a bit more interesting about the heuristic function $h$. Assume it is monotonic (consistent):

-- $h(u) \leq c + h(v)$ for every edge $(u, v, c)$ of the graph, where $c$ is the cost of the edge.

-- Provided that $h(v) = 0 for every goal vertex $v$, it is the case that monotonicity implies optimism (consistency implies admissibility).

-- We don't need a mapping of the shortest paths so far, we just need to keep a list of processed vertices. We will use a Set because that will be more efficient that using a list.

import qualified Data.Set as S
import PriorityQueue


-- Assuming that $h$ is monotonic, `mstar` will find an optimum path to a goal, provided one exists


mstar :: Graph -> Heuristic -> (Vertex -> Bool) -> Vertex -> Maybe Path
mstar g h goal source = msearch S.empty start
    where   start = insertQ ([source], 0) (h source) emptyQ;
            msearch vs ps 
                | nullQ ps = Nothing
                | goal (end p) = Just (extract p)
                | seen (end p) = msearch vs qs
                | otherwise    = msearch (S.insert (end p) vs) rs

                where   seen v  = S.member v vs;
                        (p, qs) = popQ ps;
                        rs      = addListQ (succs g h vs p) qs;


succs :: Graph -> Heuristic -> S.Set Vertex -> Path -> [(Path, Cost)]
succs g h vs p = [extend p v d | (v, d) <- g (end p), not (S.member v vs)]
                    where extend (vs, c) v d = ((v:vs, c + d), c + d + h v)


-- There is a simple set $vs$ to record vertices that have been visited to ensure that no vertex is ever processed more than once. We will show that once a path $p$ to a vertex $v$ has been found, then $p$ has the minimum cost of any path from the source to $v$, so no further paths to $v$ need to be considered.


-- Proof:

-- Suppose a path $p$ to a vertex $v$ was found before another path $p'$ to $v$. We have to show that $c(p)\leq c(p')$.

-- Let $q'$ be the initial segment of $p'$ that was on the frontier when $p$ was selected; let $q'$ end at vertex $u$ and let $r$ be the continuation of $q'$ that begins at $u$ and constitutes $p'$.

-- Then,

-- c(p) <= c(q') + h(u) - h(v)
-- Because $p$ was selected in favor of $q'$.

-- c(q') + h(u) - h(v) = c(p') - c(r) + h(u) - h(v)
-- By the definition of path costs

-- c(p') - c(r) + h(u) - h(v) <= c(p')
-- Since $h$ is monotonic and $r$ is a path from $u$ to $v$

-- TODO: We need to go over this in more detail and really understand what's happening here, I'm a bit lost.


-- This proof makes use of a generalization of monotonicity, namely that if $r$ is a path from $u$ to $v$, then $h(u) <= c(r) + h(v)$, a sort of triangle inequality.

-- Thus, `mstar` returns an optimal solution if one exists.


-- TODO Read through the example written out below

ex1_graph :: Graph
ex1_graph 'A' = [('B', 3), ('C', 10), ('D', 20), ('E', 20)]
ex1_graph 'B' = [('A', 3), ('C', 5), ('D', 8), ('E', 20)]
ex1_graph 'C' = [('A', 10), ('B', 5), ('D', 2), ('E', 10)]
ex1_graph 'D' = [('A', 20), ('B', 8), ('C', 2), ('E', 6)]
ex1_graph 'E' = [('A', 20), ('B', 20), ('C', 10), ('D', 6), ('F', 1)]
ex1_graph 'F' = [('E', 1)]
ex1_graph 'X' = [('Y', 3), ('Z', 2)]
ex1_graph 'Y' = [('X', 3), ('Z', 5)]
ex1_graph 'Z' = [('X', 2), ('Y', 5)]

ex1_heuristic :: Heuristic
ex1_heuristic v = case v of
                    'A' -> 10
                    'B' -> 10
                    'C' -> 5
                    'D' -> 5
                    'E' -> 0
                    'F' -> 0
                    'X' -> 100
                    'Y' -> 100
                    'Z' -> 100

ex1_goal :: Vertex -> Bool
ex1_goal v = v == 'F'










-- Some type definitions that we need.
type Vertex = Char -- Actually depends on the application, let's go with this though
type Cost = Integer -- Cost is always positive, so assume I wrote Nat here.

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





-- Proof of monotonicity

ex1_vertexlist :: [Vertex]
ex1_vertexlist = ['A', 'B', 'C', 'D', 'E', 'F']

ex2_vertexlist :: [Vertex]
ex2_vertexlist = ['A', 'B', 'C', 'D', 'E', 'X', 'Y', 'Z']

-- Would this convince you it's monotonic?
is_monotonic :: [Vertex] -> Heuristic -> Graph -> Bool
is_monotonic vs h g = all (id) 
                        [(h v) <= c + (h u) | v <- vs, (u, c) <- g v]
                    --   ^^^^^^^^^^^^^^^^^^ condition for monotonicity
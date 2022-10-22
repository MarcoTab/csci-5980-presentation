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

-- There is a simple set $vs$ to record vertices that have been visited to ensure that no vertex is ever processed more than once. We will show that once a path $p$ to a vertex $v$ has been found, then $p$ has the minimum cost of any path from the source to $v$, so no further paths to $v$ need to be considered.


-- Proof:

-- Suppose a path $p$ to a vertex $v$ was found before another path $p'$ to $v$. We have to show that $c(p)\leq c(p')$.

-- Let $q'$ be the initial segment of $p'$ that was on the frontier when $p$ was selected; let $q'$ end at vertex $u$ and let $r$ be the continuation of $q'$ that begins at $u$ and constitutes $p'$.

-- Then,

-- c(p) <= c(q') + h(u) - h(v)

-- Because $p$ was selected in favour of $q'$.














-- Some type definitions that we need.
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

succs :: Graph -> Heuristic -> S.Set Vertex -> Path -> [(Path, Cost)]
succs g h vs p = [extend p v d | (v, d) <- g (end p), not (S.member v vs)]
                    where extend (vs, c) v d = ((v:vs, c + d), c + d + h v)
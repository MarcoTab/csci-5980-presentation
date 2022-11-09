module PriorityQueue where

-- Heap Condition: flattening the queue returns a list of elements in ascending order of priority
-- I.e. the underlying heap is a minheap. 
-- The definition here says Fork but is meant to be read as a node. The name is to avoid clashing with other definitions.
type Rank = Integer
data PQ a p = Null | Fork Rank a p (PQ a p) (PQ a p)
                deriving (Show)

-- Each Node has a Rank value, which is the length of the shortest path from that node to a Null tree
-- These trees are leftist, that is, the rank of any left subtree will always be greater 
-- than (or equal to) the rank of any right subtree.
-- Thus, the length of the shortest path will always be along the right spine of the tree.
-- This has a bound of $\left\lfloor{log (n + 1)}\right\rfloor$


emptyQ :: PQ a p
emptyQ = Null

nullQ :: PQ a p -> Bool
nullQ Null = True
nullQ _ = False

toListQ :: Ord p => PQ a p -> [(a, p)]
toListQ Null = []
toListQ (Fork _ x p t1 t2) = (x, p) : mergeOn snd (toListQ t1) (toListQ t2)

-- Generalized merge algorithm (like the one in mergesort)
mergeOn :: Ord b => (a -> b) -> [a] -> [a] -> [a]
mergeOn key xs [] = xs
mergeOn key [] ys = ys
mergeOn key (x : xs) (y : ys)
    | key x <= key y 
        = x : mergeOn key xs (y : ys)
    | otherwise 
        = y : mergeOn key (x : xs) ys
    
rank :: PQ a p -> Rank
rank Null = 0
rank (Fork r _ _ _ _) = r

-- The following function lets us maintain rank information when constructing new Forks
fork :: a -> p -> PQ a p -> PQ a p -> PQ a p
fork x p t1 t2
    | r2 <= r1 = Fork (r2 + 1) x p t1 t2
    | otherwise = Fork (r1 + 1) x p t2 t1
        where r1 = rank t1; r2 = rank t2

-- To maintain leftist property, the two subtrees have to be swapped if the left subtree has a lower rank than the right subtree. Two leftist trees can be combined using the following function
-- TODO: How exactly does this maintain the leftist property? Unclear to me. Probably in `fork`. I think that's where that happens
combineQ :: Ord p => PQ a p -> PQ a p -> PQ a p
combineQ Null t = t
combineQ t Null = t
combineQ (Fork k1 x1 p1 l1 r1) (Fork k2 x2 p2 l2 r2)
    | p1 <= p2 = fork x1 p1 l1 (combineQ r1 (Fork k2 x2 p2 l2 r2))
    | otherwise = fork x2 p2 l2 (combineQ (Fork k1 x1 p1 l1 r1) r2)
-- Analysis time!!!!! Runtime? Worst case is that combineQ traverses the right spine of both queues. So the runtime of combineQ over two trees of rank at most $r$ must be $O(log r)$.

-- Next, define deleteQ and insertQ
insertQ :: Ord p => a -> p -> PQ a p -> PQ a p
insertQ x p t = combineQ (fork x p Null Null) t

-- I think the assumption here is that you'd use nullQ before trying popQ every time
deleteQ :: Ord p => PQ a p -> ((a,p), PQ a p)
deleteQ (Fork _ x p t1 t2) = ((x,p), combineQ t1 t2)

-- popQ is deleteQ but we don't keep the priority
popQ :: Ord p => PQ a p -> (a, PQ a p)
popQ q1 = (x, q2) 
    where ((x,_), q2) = deleteQ q1 

-- deleteQ and insertQ are both $O(log n)$.

addListQ :: Ord p => [(a,p)] -> PQ a p -> PQ a p
addListQ xs q = foldr (uncurry insertQ) q xs

removeQ :: Ord p => PQ a p -> (a, PQ a p)
removeQ q1 = (x,q2) where ((x,_),q2) = deleteQ q1
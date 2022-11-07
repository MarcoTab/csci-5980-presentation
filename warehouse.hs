import qualified Data.Set as S
import qualified Data.Array as A
import PriorityQueue

type Coord = Nat
type Vertex = (Coord,Coord)
type Box = Vertex --box identified by its top-left vertex
type Grid = (Nat,Nat,[Box])
type Graph = Vertex -> [Vertex]
type Segment = (Vertex,Vertex)
type Dist = Float
type Path = ([Vertex],Dist)

-- lower-order functions for all algorithms
dist :: Vertex -> Vertex -> Dist
dist (x1,y1) (x2,y2) = sqrt (fromIntegral (sqr(x2-x1) + sqr(y2-y1)))
                     where sqr x = x*x
                     
boxes :: Grid -> [Box]
boxes (_,_,bs) = bs

corners :: Box -> [Vertex]
corners (x,y) = [(x,y),(x+1,y),(x+1,y-1),(x,y-1)]

end :: Path -> Vertex
end = head . fst

extract :: Path -> Path
extract (vs,d) = (reverse vs,d)

free :: Grid -> Vertex -> Bool
free (m,n,bs) = (a A.!)
    where a = A.listArray ((0,0),(m+1,n+1)) (repeat True)
              A.//[((x,y),False)|x<-[0..m+1],y<-[0,n+1]] 
              A.//[((x,y),False)|x<-[0,m+1],y<-[1..n]] 
              A.//[((x,y),False)|b <-bs,(x,y)<-corners b]

-- fixed-angle sol'n : neighbors of a vertex are unoccupied / non-boundary adjacent grid points
neighborsf :: Grid -> Graph
neighborsf grid = filter (free grid) . adjacentsf

adjacentsf :: Vertex -> [Vertex]
adjacentsf (x,y) = [(x-1,y-1),(x-1,y),(x-1,y+1),(x,y-1),(x,y+1),(x+1,y-1),(x+1,y),(x+1,y+1)]

fpath :: Grid -> Vertex -> Vertex -> Maybe Path
fpath grid = mstarf (neighborsf grid)

succsf :: Graph -> Vertex -> S.Set Vertex -> Path -> [(Path,Dist)]
succsf g target visited p =
    [extend p v | v <- g (end p), not (S.member v visited)]
    where extend (u:vs,d) v = ((v:u:vs,dv),dv+dist v target)
                            where dv = d + dist u v

-- mstar implementation for a fixed-angle path
mstarf :: Graph -> Vertex -> Vertex -> Maybe Path
mstarf g goal source = msearch S.empty start
                      where start = insertQ ([source],0) 0 emptyQ
                            msearch vs ps | nullQ ps = Nothing
                                          | goal == end p = Just (extract p)
                                          | seen (end p) = msearch vs qs
                                          | otherwise = msearch (S.insert (end p) vs) rs
                                          where seen v = S.member v vs 
                                                (p,qs) = removeQ ps
                                                rs = addListQ (succsf g goal vs p) qs

-- lower-order functions for variable angle paths
hseg :: Segment -> Bool
hseg ((x1,y1),(x2,y2)) = x1 == x2
vseg :: Segment -> Bool
vseg ((x1,y1),(x2,y2)) = y1 == y2
dseg :: Segment -> Bool
dseg ((x1,y1),(x2,y2)) = (x1 + y1) == (x2 + y2)
eseg :: Segment -> Bool
eseg ((x1,y1),(x2,y2)) = (x1 - y1) == (x2 - y2)

ypoints :: Segment -> [Vertex]
ypoints ((x1,y1),(x2,y2)) = [(x1,y) | y <- [min y1 y2 .. max y1 y2]]
xpoints :: Segment -> [Vertex]
xpoints ((x1,y1),(x2,y2)) = [(x,y1) | x <- [min x1 x2 .. max x1 x2]]
dpoints :: Segment -> [Vertex]
dpoints ((x1,y1),(x2,y2)) = [(x,x1+y1-x) | x <- [min x1 x2 .. max x1 x2]]
epoints :: Segment -> [Vertex]
epoints ((x1,y1),(x2,y2)) = [(x1-y1+y,y) | y <- [min y1 y2 .. max y1 y2]]

near :: Segment -> Segment -> Bool
near ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) =  min x1 x2 <= x3
                                         && (min y1 y2 <= y3)
                                         && (max x1 x2 >= x4)
                                         && (max y1 y2 >= y4)

borders :: Grid -> [Segment]
borders = concatMap (edges . corners) . boxes
        where edges [u,v,w,x] = [(u,v),(w,v),(x,w),(x,u)]
        

crosses :: Segment -> (Vertex, Vertex) -> Bool
crosses s (v1, v2) = orientation s v1 * orientation s v2 <= 0

orientation :: Segment -> Vertex -> Int
orientation ((x1,y1),(x2,y2)) (x,y)
 = signum((x-x1)*(y2-y1)-(x2-x1)*(y-y1))

-- variable-angle sol'n: 
visible :: Grid -> Segment -> Bool
visible g s | hseg s = all (free g) (ypoints s)
            | vseg s = all (free g) (xpoints s)
            | dseg s = all (free g) (dpoints s)
            | eseg s = all (free g) (epoints s)
            | otherwise = free g (snd s) && (not . any (crosses s)) es
            where es = filter (near s) (borders g)
                                         
neighborsv :: Grid -> Vertex -> [Vertex]
neighborsv (m,n,bs) (x1,y1) =
 [(x2,y2) | x2 <- [1..m], y2 <- [1..n], visible (m,n,bs) ((x1,y1),(x2,y2))]

-- variable-angle path v1: neighbors are ALL visible points on grid, not just adjacent ones
vpath1 :: Grid -> Vertex -> Vertex -> Maybe Path
vpath1 grid = mstarf (neighborsv grid)

-- variable-angle path v2: neighbors are all adjacent points on grid, but if the parent p of the
-- current last point u in the path is visible from the point v to be added, delete pu and add pv
vpath2 :: Grid -> Vertex -> Vertex -> Maybe Path
vpath2 grid = 
    mstarv (neighborsf grid) (visible grid)
    
succsv :: (Vertex -> [Vertex]) -> (Segment -> Bool) -> Vertex -> S.Set Vertex -> Path -> [(Path, Dist)]
succsv g vtest target vs p =
    [extend p w|w <- g (end p), not(S.member w vs)]
    where extend (v:vs,d) w = if not (null vs) && vtest (u,w)
                              then ((w:vs,du),du+dist w target)
                              else ((w:v:vs,dw),dw+dist w target)
                              where u = head vs
                                    du = d - dist u v + dist u w
                                    dw = d + dist v w

-- mstar implementation for a smoothing path
mstarv :: Graph -> (Segment -> Bool) -> Vertex -> Vertex -> Maybe Path
mstarv g vtest goal source = msearch S.empty start
                      where start = insertQ ([source],0) 0 emptyQ
                            msearch vs ps | nullQ ps      = Nothing
                                          | goal == end p = Just (extract p)
                                          | seen (end p)  = msearch vs qs
                                          | otherwise     = msearch (S.insert (end p) vs) rs
                                          where seen v = S.member v vs 
                                                (p,qs) = removeQ ps
                                                rs     = addListQ (succsv g vtest goal vs p) qs

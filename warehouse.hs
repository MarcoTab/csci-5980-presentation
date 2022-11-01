import qualified Data.Set as S

type Nat = Int
type Coord = Nat
type Vertex = (Coord,Coord)
type Box = Vertex --box identified by its top-left vertex
type Grid = (Nat,Nat,[Box])
type Graph = Vertex -> [Vertex]
type Segment = (Vertex,Vertex)

boxes :: Grid -> [Box]
boxes (_,_,bs) = bs

corners :: Box -> [Vertex]
corners (x,y) = [(x,y),(x+1,y),(x+1,y-1),(x,y-1)]

-- fixed-angle sol'n : neighbors of a vertex are unoccupied / non-boundary adjacent grid points
neighborsf :: Grid -> Graph
neighborsf grid = filter (free grid) . adjacentsf

adjacentsf :: Vertex -> [Vertex]
adjacentsf (x,y) = [(x−1,y−1),(x−1,y),(x−1,y+1),(x,y−1),(x,y+1),(x+1,y−1),(x+1,y),(x+1,y+1)]

free :: Grid -> Vertex -> Bool
free (m,n,bs) = (a!)
    where a = listArray ((0,0),(m+1,n+1)) (repeat True)
              //[((x,y),False)|x<-[0..m+1],y<-[0,n+1]] 
              //[((x,y),False)|x<-[0,m+1],y<-[1..n]] 
              //[((x,y),False)|b <-bs,(x,y)<-corners b]

type Dist = Float
type Path = ([Vertex],Dist)

end :: Path -> Vertex
end = head . fst

extract :: Path -> Path
extract (vs,d) = (reverse vs,d)

-- fixed-angle path
fpath :: Grid -> Vertex -> Vertex -> Maybe Path
fpath grid = mstarf (neighboursf grid)

-- mstar implementation for a fixed-angle path
mstarf :: Graph -> Vertex -> Vertex -> Maybe Path
mstarf g goal source = msearch S.empty start
                      where start = insertQ ([source],0) 0 emptyQ
                            msearch vs ps | nullQ ps = Nothing
                                          | goal (end p) = Just (extract p)
                                          | seen (end p) = msearch vs qs
                                          | otherwise = msearch (S.insert (end p) vs) rs
                                          where seen v = S.member v vs 
                                                (p,qs) = removeQ ps
                                                rs = addListQ (succsf g goal vs p) qs

succsf :: Graph -> Vertex -> S.Set Vertex -> Path -> Maybe Path
succsf g target visited p =
    [extend p v | v <- g (end p), not (S.member v visited)]
    where extend (u:vs,d) v = ((v:u:vs,dv),dv+dist v target)
                            where dv = d + dist u v
                            
dist :: Vertex -> Vertex -> Dist
dist (x1,y1) (x2,y2) = sqrt (fromIntegral (sqr(x2-x1) + sqrt(y2-y1)))
                     where sqr x = x*x
                     
--f'n to determine whether a segment (line b/w a vertex/vertex pair) is impeded by a box
visible :: Grid -> Segment -> Bool
visible g s = 

--variable-angle path
vpath :: Grid -> Vertex -> Vertex -> Maybe Path
vpath grid = 
    mstarv (neighborsv grid) (visible grid)
    
-- mstar implementation for a variable-angle path
mstarv :: Graph -> (Segment -> Bool) -> Vertex -> Vertex -> Maybe Path
mstarv g h goal source = msearch S.empty start
                      where start = insertQ ([source],0) (h source) emptyQ
                            msearch vs ps | nullQ ps = Nothing
                                          | goal (end p) = Just (extract p)
                                          | seen (end p) = msearch vs qs
                                          | otherwise = msearch (S.insert (end p) vs) rs
                                          where seen v = S.member v vs 
                                                (p,qs) = removeQ ps
                                                rs = addListQ (succs g h vs p) qs


succsv :: Graph -> (Grid -> Bool) -> Vertex -> S.Set Vertex -> Path -> Maybe Path
succsv g vtest target vs p =
    [extend p w|w <- g (end p), not(S.member w vs)]
    where extend (v:vs,d) w = if not (null vs) && vtest (u,w)
                              then ((w:vs,du),du+dist w target)
                              else ((w:v:vs,dw),dw+dist w target)
                              where u = head vs
                                    du = d - dist u v + dist u w
                                    dw = d + dist v w
                                    
vtest = visible grid

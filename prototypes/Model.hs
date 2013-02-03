{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Model
       (Point, Direction (..), dirReverse, (^$^),
        LineString, makeGeometry, liftG, moveGeometry, snapGeometry, normalizeBend,
        moveBend, addBend,
        Distance (..),
        Model, empty, star,
        modelNodes, modelEdges, modelEdgeEnds, modelEdgeDiamonds, modelEdgeBranches,
        Element, ElementLabel (..), PointElement (..), LinearElement (..), elementId,
        EdgeElement (..),
        Node (), nodeXY, nodeEnds, nodeEdges, nodeDiamonds, nodeBranches, nodeRadius,
        Edge (), edgeGeometry, edgeEnds, edgeNodes,
        EdgeDiamond (), diamondXY, diamondBranches, diamondNodes,
        EdgeEnd (), endDirection, endXY, endNode, endEdge, endOther,
        EdgeBranch (), branchGeometry, branchXY, branchNode, branchDiamond, branchOthers,
        HitTest (..), hitTest,
        ModelAction, idModel,
        addNode, moveNode, deleteNode,
        addEdge, addBranch,
        splitEdge', splitEdge, splitAtBend,
        makeBranchFromEdge, makeEdgeFromBranch,
        setEdgeGeometry, setBranchGeometry,
        rerouteEdge, rerouteBranch,
        reconnectEdgeEnd, reconnectBranchNode, reconnectBranchDiamond,
        moveEdgeDiamond, deleteEdge) where

import qualified Control.Arrow as Ar
import Control.Arrow ((&&&), (***), (>>>))
import Control.Applicative ((<$>), (<*>), pure)
import qualified Control.Monad.State.Lazy as S
import Data.Function (on)
import qualified Data.Graph.Inductive as G
import qualified Data.List as L
import qualified Data.Maybe as Mb
import Data.Ord (comparing)
import qualified Data.VectorSpace as V
import Data.VectorSpace ((^+^), (^-^), (<.>))
import qualified GHC.Exts as Ext

import Debug.Trace

trace' :: Show a => a -> a
trace' x = traceShow x x

{- Auxiliary: Lists -}

insertBefore :: Int -> a -> [a] -> [a]
insertBefore i x xs = splitAt i >>> Ar.second (x:) >>> uncurry (++) $ xs

deleteAt :: Int -> [a] -> [a]
deleteAt i xs = splitAt i >>> Ar.second tail >>> uncurry (++) $ xs

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f xs = let (before, at : after) = splitAt i xs
                  in before ++ f at : after

{- Auxiliary: Geometry -}

nodeRadius :: Double
nodeRadius = 10

data Direction = Forward | Reverse deriving (Eq, Ord, Show)

dirReverse :: Direction -> Direction
dirReverse Forward = Reverse
dirReverse Reverse = Forward

geomEnd :: Direction -> LineString -> Point
geomEnd Forward = head
geomEnd Reverse = last

(^$^) :: Direction -> LineString -> LineString
Forward ^$^ g = g
Reverse ^$^ g = reverse g

type Point = (Double, Double)
type Segment = (Point, Point)
type LineString = [Point]

class Distance a where
    distance :: Point -> a -> Double
    distance xy a = V.magnitude (xy ^-^ projection xy a)
    projection :: Point -> a -> Point
instance Distance Point where
    projection _ xy' = xy'
instance Distance Segment where
    projection xy (a, b) = let l2 = V.magnitudeSq $ b ^-^ a
                               t = ((xy ^-^ a) <.> (b ^-^ a)) / l2
                           in if l2 == 0 || t < 0 then a
                              else if t > 1 then b
                                   else V.lerp a b t
instance Distance LineString where
    distance xy geom = minimum . map (distance xy) $ segments geom
    projection xy geom = snd . head . Ext.sortWith fst
                         . map (distance xy &&& projection xy) $ segments geom

liftG :: Direction -> (LineString -> LineString) -> LineString -> LineString
liftG Forward f = f
liftG Reverse f = reverse . f . reverse

segments :: LineString -> [Segment]
segments ls = ls `zip` tail ls

unsegments :: [Segment] -> LineString
unsegments ss = fst (head ss) : map snd ss

makeGeometry :: (Point, Double) -> (Point, Double) -> LineString
makeGeometry ((x0, y0), r0) ((x1, y1), r1) = [(x0', y0'), (x1', y1')]
    where len = distance (x1, y1) (x0, y0)
          (x0', y0') = V.lerp (x0, y0) (x1, y1) (r0/len)
          (x1', y1') = V.lerp (x1, y1) (x0, y0) (r1/len)

snapGeometry :: (Point, Double) -> (Point, Double) -> LineString -> LineString
snapGeometry xyr0 xyr1 g
    | length g > 2 = snapHead xyr0 . liftG Reverse (snapHead xyr1) $ g
    | otherwise    = makeGeometry xyr0 xyr1
snapGeometry_ :: Point -> Point -> LineString -> LineString
snapGeometry_ xy0 xy1 = snapGeometry (xy0, nodeRadius) (xy1, nodeRadius)

snapHead :: (Point, Double) -> LineString -> LineString
snapHead (xy0, r0) (_:xys@(xy:_)) = V.lerp xy0 xy (r0 / distance xy xy0) : xys
snapHead_ :: Point -> LineString -> LineString
snapHead_ xy = snapHead (xy, nodeRadius)

moveGeometry :: Point -> LineString -> LineString
moveGeometry dxy = map (^+^ dxy)

-- Split a linestring at a point lying on it. Reverse the first half.
splitGeometry :: Point -> LineString -> (LineString, LineString)
splitGeometry xy g
    | xy `elem` g = (xy : reverse us, vs)
    | otherwise = (xy : map snd (reverse ss1) ++ [head g],
                   xy : map fst ss2 ++ [last g])
    where
        (us, vs) = break (== xy) g
        (ss1, _ : ss2) = span ((> distance xy g) . distance xy) $ segments g

normalizeBend :: Int -> LineString -> LineString
normalizeBend i g
    | any (near g0) [gm1, gp1] || flat gm1 g0 gp1 = deleteAt i g
    | i+2 < length g && flat g0 gp1 gp2 = deleteAt (i+1) g
    | i-2 >= 0 && flat gm2 gm1 g0 = deleteAt (i-1) g
    | otherwise = g
    where [gm2, gm1, g0, gp1, gp2] = [g !! (i + k) | k <- [-2..2]]
          near xy xy' = distance xy xy' < nodeRadius
          flat a b c = abs (phi (b ^-^ a) - phi (c ^-^ b)) < pi/18
          phi (dx, dy) = atan2 dy dx

moveBend :: Int -> Point -> LineString -> LineString
moveBend i xy g = modifyAt i (const xy) g

addBend :: Int -> Point -> LineString -> LineString
addBend i xy g = insertBefore i xy g


{- Auxiliary: Graph operations -}

mapNodes :: G.DynGraph gr => (G.LNode a -> a) -> gr a b -> gr a b
mapNodes f g = if G.isEmpty g then g
               else let ((inLinks, nodeId, nodeLabel, outLinks), g') = G.matchAny g
                    in (inLinks, nodeId, f (nodeId, nodeLabel), outLinks) G.& mapNodes f g'

node :: G.Graph gr => gr a b -> G.Node -> G.LNode a
node g n = (n, l) where Just l = G.lab g n

modifyNodeLabel :: G.DynGraph gr => G.Node -> (G.LNode a -> a) -> gr a b -> gr a b
modifyNodeLabel n f g = modify' $ G.match n g
    where modify' (Nothing, g') = g'
          modify' (Just (ins, _, l, outs), g') = (ins, n, f (n, l), outs) G.& g'


labNeighbors :: G.Graph gr => gr a b -> G.Node -> [G.LNode a]
labNeighbors g n = map (node g) $ G.neighbors g n

labNeighborsL :: G.Graph gr => gr a b -> G.LNode a -> [G.LNode a]
labNeighborsL g (n, _) = labNeighbors g n

{- The Model -}

data Model = Model Graph deriving (Show)

empty :: Model
empty = Model G.empty

star :: Model
star = S.execState (addEdges =<< addNodes) empty
    where points :: [Point]
          points = [(x, y) | i <- [0..4] :: [G.Node],
                    let x = 300 + 200 * sin (2*pi/5 * fromIntegral i)
                        y = 225 - 200 * cos (2*pi/5 * fromIntegral i)]
          addNodes :: ModelAction [Node]
          addNodes = mapM addNode points
          addEdges :: [Node] -> ModelAction [(Edge, [EdgeEnd])]
          addEdges ns = S.zipWithM addEdge' ns (drop 2 $ cycle ns)
          addEdge' :: Node -> Node -> ModelAction (Edge, [EdgeEnd])
          addEdge' u v = addEdge Forward u v =<< geom u v
          geom :: Node -> Node -> ModelAction LineString
          geom u v = do
              uxy <- S.gets (`nodeXY` u)
              vxy <- S.gets (`nodeXY` v)
              return $ makeGeometry (uxy, 10) (vxy, 10)


type Graph = G.Gr ElementLabel ()

data ElementLabel = NodeLabel Point -- center of circle
                  | EdgeLabel LineString -- from Forward end to Reverse end
                  | EdgeDiamondLabel Point -- center of diamond
                  | EdgeEndLabel Direction
                  | EdgeBranchLabel LineString -- from diamond to node
                  deriving (Eq, Ord, Show)

type Element = G.LNode ElementLabel
newtype Node = Node Element deriving (Show)
newtype Edge = Edge Element deriving (Show)
newtype EdgeEnd = EdgeEnd Element deriving (Show)
newtype EdgeDiamond = EdgeDiamond Element deriving (Show)
newtype EdgeBranch = EdgeBranch Element deriving (Show)

class    ElementClass elt         where toElement :: elt -> Element
instance ElementClass Element     where toElement                 = id
instance ElementClass Node        where toElement (Node e)        = e
instance ElementClass Edge        where toElement (Edge e)        = e
instance ElementClass EdgeEnd     where toElement (EdgeEnd e)     = e
instance ElementClass EdgeDiamond where toElement (EdgeDiamond e) = e
instance ElementClass EdgeBranch  where toElement (EdgeBranch e)  = e

elementId :: ElementClass elt => elt -> G.Node
elementId = fst . toElement

elementLabel :: ElementClass elt => elt -> ElementLabel
elementLabel = snd . toElement

instance Distance Edge where
    projection xy (Edge (_, EdgeLabel geom)) = projection xy geom
instance Distance EdgeDiamond where
    projection _ (EdgeDiamond (_, EdgeDiamondLabel xy)) = xy

class PointElement a where
    getXY :: Model -> a -> Point
    moveXY :: a -> Point -> ModelAction ()
instance PointElement Node where
    getXY = nodeXY
    moveXY = moveNode
instance PointElement EdgeDiamond where
    getXY = diamondXY
    moveXY = moveEdgeDiamond

class LinearElement a where
    getGeometry :: Model -> a -> LineString
    rerouteGeometry :: a -> LineString -> ModelAction ()
instance LinearElement Edge where
    getGeometry = edgeGeometry
    rerouteGeometry = rerouteEdge
instance LinearElement EdgeBranch where
    getGeometry = branchGeometry
    rerouteGeometry = rerouteBranch

class EdgeElement a where delete :: a -> ModelAction ()
instance EdgeElement Edge where delete = deleteEdge
instance EdgeElement EdgeDiamond where delete = deleteEdgeDiamond

(===) :: ElementClass elt => elt -> elt -> Bool
(===) = (==) `on` elementId

isNode :: Element -> Bool
isNode (_, NodeLabel _) = True
isNode _ = False

isEdge :: Element -> Bool
isEdge (_, EdgeLabel _) = True
isEdge _ = False

isEdgeDiamond :: Element -> Bool
isEdgeDiamond (_, EdgeDiamondLabel _) = True
isEdgeDiamond _ = False

isEdgeEnd :: Element -> Bool
isEdgeEnd (_, EdgeEndLabel _) = True
isEdgeEnd _ = False

isEdgeBranch :: Element -> Bool
isEdgeBranch (_, EdgeBranchLabel _) = True
isEdgeBranch _ = False


elementNeighbors :: ElementClass elt => Model -> elt -> [Element]
elementNeighbors (Model g) elt = labNeighbors g $ elementId elt

modelElements :: Model -> [Element]
modelElements (Model g) = G.labNodes g

modelNodes :: Model -> [Node]
modelNodes = map Node . filter isNode . modelElements

modelEdges :: Model -> [Edge]
modelEdges = map Edge . filter isEdge . modelElements

modelEdgeDiamonds :: Model -> [EdgeDiamond]
modelEdgeDiamonds = map EdgeDiamond . filter isEdgeDiamond . modelElements

modelEdgeEnds :: Model -> [EdgeEnd]
modelEdgeEnds = map EdgeEnd . filter isEdgeEnd . modelElements

modelEdgeBranches :: Model -> [EdgeBranch]
modelEdgeBranches = map EdgeBranch . filter isEdgeBranch . modelElements


nodeXY :: Model -> Node -> Point
nodeXY _ (Node (_, NodeLabel xy)) = xy

nodeEnds :: Model -> Node -> [EdgeEnd]
nodeEnds m = map EdgeEnd . filter isEdgeEnd . elementNeighbors m

nodeEdges :: Model -> Node -> [Edge]
nodeEdges m = map (endEdge m) . nodeEnds m

nodeBranches :: Model -> Node -> [EdgeBranch]
nodeBranches m = map EdgeBranch . filter isEdgeBranch . elementNeighbors m

nodeDiamonds :: Model -> Node -> [EdgeDiamond]
nodeDiamonds m = map (branchDiamond m) . nodeBranches m


edgeGeometry :: Model -> Edge -> LineString
edgeGeometry _ (Edge (_, EdgeLabel geom)) = geom

edgeEnds :: Model -> Edge -> [EdgeEnd]
edgeEnds m = map EdgeEnd . Ext.sortWith snd . filter isEdgeEnd . elementNeighbors m

edgeEnd :: Model -> Direction -> Edge -> EdgeEnd
edgeEnd m dir = head . filter ((== dir) . endDirection m) . edgeEnds m

edgeNodes :: Model -> Edge -> [Node]
edgeNodes m = map (endNode m) . edgeEnds m


diamondXY :: Model -> EdgeDiamond -> Point
diamondXY _ (EdgeDiamond (_, EdgeDiamondLabel xy)) = xy

diamondBranches :: Model -> EdgeDiamond -> [EdgeBranch]
diamondBranches m = map EdgeBranch . filter isEdgeBranch . elementNeighbors m

diamondNodes :: Model -> EdgeDiamond -> [Node]
diamondNodes m = map (branchNode m) . diamondBranches m


endDirection :: Model -> EdgeEnd -> Direction
endDirection _ (EdgeEnd (_, EdgeEndLabel dir)) = dir

endXY :: Model -> EdgeEnd -> Point
endXY m end = geomEnd (endDirection m end) . edgeGeometry m $ endEdge m end

endNode :: Model -> EdgeEnd -> Node
endNode m = Node . head . filter isNode . elementNeighbors m

endEdge :: Model -> EdgeEnd -> Edge
endEdge m = Edge . head . filter isEdge . elementNeighbors m

endOther :: Model -> EdgeEnd -> EdgeEnd
endOther m end = head . L.deleteBy (===) end . edgeEnds m $ endEdge m end


branchGeometry :: Model -> EdgeBranch -> LineString
branchGeometry _ (EdgeBranch (_, EdgeBranchLabel geom)) = geom

branchXY :: Model -> EdgeBranch -> Point
branchXY m = last . branchGeometry m

branchNode :: Model -> EdgeBranch -> Node
branchNode m = Node . head . filter isNode . elementNeighbors m

branchDiamond :: Model -> EdgeBranch -> EdgeDiamond
branchDiamond m = EdgeDiamond . head . filter isEdgeDiamond . elementNeighbors m

branchOthers :: Model -> EdgeBranch -> [EdgeBranch]
branchOthers m b = L.deleteBy (===) b . diamondBranches m $ branchDiamond m b


idE :: ElementClass elt => (elt, context) -> Maybe ElementLabel
idE = const Nothing

mapElements :: ((Node, ()) -> Maybe ElementLabel) -- labeled node -> new label
               -> ((EdgeEnd, (Node, Edge)) -> Maybe ElementLabel) -- end, node, edge -> new label
               -> ((Edge, [(EdgeEnd, Node)]) -> Maybe ElementLabel)
                   -- edge, [(Forward end, Forward node), (Reverse end, Reverse node)]
               -> ((EdgeBranch, (Node, EdgeDiamond)) -> Maybe ElementLabel)
               -> ((EdgeDiamond, [(EdgeBranch, Node)]) -> Maybe ElementLabel)
               -> Model -> Model
mapElements fNode fEnd fEdge fBranch fDiamond m@(Model g) = Model $ mapNodes f g
    where f elt
              | isNode elt = label' $ fNode (Node elt, ())
              | isEdge elt = let
                  edge = Edge elt
                  endsNodes = map (id &&& endNode m) $ edgeEnds m edge
                  in label' $ fEdge (edge, endsNodes)
              | isEdgeEnd elt = let
                  end = EdgeEnd elt
                  node = endNode m end
                  edge = endEdge m end
                  in label' $ fEnd (end, (node, edge))
              | isEdgeDiamond elt = let
                  diamond = EdgeDiamond elt
                  branchesNodes = map (id &&& branchNode m) $ diamondBranches m diamond
                  in label' $ fDiamond (diamond, branchesNodes)
              | isEdgeBranch elt = let
                  branch = EdgeBranch elt
                  node = branchNode m branch
                  diamond = branchDiamond m branch
                  in label' $ fBranch (branch, (node, diamond))
              where label' :: Maybe ElementLabel -> ElementLabel
                    label' = Mb.fromMaybe (elementLabel elt)


data HitTest = OnNode          Node
             | OnEdgeSegment   Edge Int -- 0 to n-2 where n = length geom
             | OnEdgeBend      Edge Int -- 1 to n-2; 0th and (n-1)th report OnEdgeEnd
             | OnEdgeDiamond   EdgeDiamond
             | OnEdgeEnd       EdgeEnd
             | OnBranchStart   EdgeBranch
             | OnBranchSegment EdgeBranch Int -- 0 to n-2 outwards
             | OnBranchBend    EdgeBranch Int -- 1 to n-2 outwards
             | OnBranchEnd     EdgeBranch
             | Nowhere
             deriving (Show)

distanceTo :: Model -> Point -> HitTest -> (Int, Double)
distanceTo m xy (OnNode n)            = (0, distance xy $ nodeXY m n)
distanceTo m xy (OnEdgeDiamond d)     = (0, distance xy $ diamondXY m d)
distanceTo m xy (OnEdgeEnd e)         = (1, distance xy $ endXY m e)
distanceTo m xy (OnBranchStart b)     = (1, distance xy . head $ branchGeometry m b)
distanceTo m xy (OnBranchEnd b)       = (1, distance xy . last $ branchGeometry m b)
distanceTo m xy (OnEdgeBend e i)      = (2, distance xy $ edgeGeometry m e !! i)
distanceTo m xy (OnBranchBend b i)    = (2, distance xy $ branchGeometry m b !! i)
distanceTo m xy (OnEdgeSegment e i)   = (3, distance xy $ segments (edgeGeometry m e) !! i)
distanceTo m xy (OnBranchSegment b i) = (3, distance xy $ segments (branchGeometry m b) !! i)
distanceTo _  _  Nowhere              = (9, 0)

hitTest :: Model -> Point -> HitTest
hitTest m xy = fst
                . L.minimumBy (comparing snd)
               . filter ((< 10) . snd . snd)
               . map (id &&& distanceTo m xy)
               $ possibilities
    where possibilities =
              Nowhere
              : map OnNode (modelNodes m)
              ++ concatMap edgePossibilities (modelEdges m)
              ++ map OnEdgeDiamond (modelEdgeDiamonds m)
              ++ map OnEdgeEnd (modelEdgeEnds m)
              ++ concatMap branchPossibilities (modelEdgeBranches m)
          edgePossibilities e =
              map (OnEdgeBend e) [1..n - 2]
              ++ map (OnEdgeSegment e) [0..n - 2]
              where n = length $ edgeGeometry m e
          branchPossibilities b =
              OnBranchStart b
              : OnBranchEnd b
              : map (OnBranchBend b) [1..n - 2]
              ++ map (OnBranchSegment b) [0..n - 2]
              where n = length $ branchGeometry m b


type ModelAction r = S.State Model r

idModel :: ModelAction ()
idModel = return ()

modelGraph :: Model -> Graph
modelGraph (Model g) = g

getGraph :: ModelAction Graph
getGraph = S.gets modelGraph

modifyGraph :: (Graph -> Graph) -> ModelAction ()
modifyGraph = S.modify . liftModel
    where liftModel f (Model g) = Model $ f g


addNode :: Point -> ModelAction Node
addNode xy = do
    [n] <- G.newNodes 1 <$> getGraph
    let ne = (n, NodeLabel xy)
    modifyGraph $ G.insNode ne
    return $ Node ne

moveNode :: Node -> Point -> ModelAction ()
moveNode (Node (n, _)) xy' = S.modify $ mapElements fixNode idE fixEdge fixBranch idE
    where fixNode (Node (u, _), ())
              | u == n = Just $ NodeLabel xy'
              | otherwise = Nothing
          fixEdge (Edge (_, EdgeLabel geom),
                   [(_, Node (u, NodeLabel uxy)), (_, Node (v, NodeLabel vxy))])
              | u == n && v == n = Just . EdgeLabel $ moveGeometry (xy' ^-^ uxy) geom
              | u == n     = Just . EdgeLabel $ snapGeometry_ xy' vxy geom
              | v == n     = Just . EdgeLabel $ snapGeometry_ uxy xy' geom
              | otherwise = Nothing
          fixBranch (EdgeBranch (_, EdgeBranchLabel geom), (Node (u, _), _))
              | u == n     = Just . EdgeBranchLabel $ liftG Reverse (snapHead_ xy') geom
              | otherwise = Nothing

deleteNode :: Node -> ModelAction ()
deleteNode n = do
    m <- S.get
    let edges = nodeEdges m n
        diamonds = nodeDiamonds m n
        ends = concatMap (edgeEnds m) edges
        branches = nodeBranches m n
        ternaryDiamonds = filter ((3 ==) . length . diamondBranches m) diamonds
    modifyGraph . G.delNodes . map fst
        $ toElement n
        : map toElement edges
        ++ map toElement ends 
        ++ map toElement branches
    mapM_ normalizeDiamond ternaryDiamonds


normalizeDiamond :: EdgeDiamond -> ModelAction ()
normalizeDiamond dd@(EdgeDiamond (d, EdgeDiamondLabel xy)) = do
    branches <- S.gets diamondBranches <*> pure dd
    case branches of
        [EdgeBranch (ud, EdgeBranchLabel udgeom),
         EdgeBranch (vd, EdgeBranchLabel vdgeom)] ->
            modifyGraph
            $ modifyNodeLabel  d (const $ EdgeLabel geom')
            . modifyNodeLabel ud (const $ EdgeEndLabel Forward)
            . modifyNodeLabel vd (const $ EdgeEndLabel Reverse)
            where
                geom' = normalizeBend (length before) $ before ++ xy : after
                before = reverse $ tail udgeom
                after = tail vdgeom
        _ -> return ()

addEdge :: Direction -> Node -> Node -> LineString -> ModelAction (Edge, [EdgeEnd])
addEdge Reverse v u geom = addEdge Forward u v (reverse geom)
addEdge Forward (Node (u, NodeLabel uxy)) (Node (v, NodeLabel vxy)) geom = do
    [e, ue, ve] <- G.newNodes 3 <$> getGraph
    let ee = (e, EdgeLabel $ snapGeometry_ uxy vxy geom)
        uee = (ue, EdgeEndLabel Forward)
        vee = (ve, EdgeEndLabel Reverse)
    modifyGraph
        $ G.insEdges [(ue, u, ()), (ue, e, ()), (ve, v, ()), (ve, e, ())]
        . G.insNodes [ee, uee, vee]
    return (Edge ee, map EdgeEnd [uee, vee])

splitEdge' :: Edge -> Point -> ModelAction EdgeDiamond
splitEdge' ee@(Edge (e, EdgeLabel geom)) xy = do
    [EdgeEnd (ue, _), EdgeEnd (ve, _)] <- S.gets edgeEnds <*> pure ee
    let exy = projection xy geom
        (uegeom, vegeom) = snapHead_ exy *** snapHead_ exy $ splitGeometry exy geom
    modifyGraph
        $ modifyNodeLabel  e (const $ EdgeDiamondLabel exy)
        . modifyNodeLabel ue (const $ EdgeBranchLabel uegeom)
        . modifyNodeLabel ve (const $ EdgeBranchLabel vegeom)
    return $ EdgeDiamond (e, EdgeDiamondLabel exy)

splitEdge :: Edge -> (EdgeDiamond -> a -> LineString -> ModelAction r)
             -> a -> LineString -> ModelAction r
splitEdge e ma a geom = do
    e' <- splitEdge' e $ head geom
    ma e' a geom

splitAtBend :: Edge -> Int -> (EdgeDiamond -> a -> LineString -> ModelAction r)
              -> a -> LineString -> ModelAction r
splitAtBend e@(Edge (_, EdgeLabel egeom)) i ma a geom =
    splitEdge e ma a $ snapHead (egeom !! i, 0) geom

addBranch :: EdgeDiamond -> Node -> LineString -> ModelAction EdgeBranch
addBranch (EdgeDiamond (d, EdgeDiamondLabel dxy)) (Node (w, NodeLabel wxy)) wdgeom = do
    [wd] <- G.newNodes 1 <$> getGraph
    let wde = (wd, EdgeBranchLabel $ snapGeometry_ dxy wxy wdgeom)
    modifyGraph
        $ G.insEdges [(wd, w, ()), (wd, d, ())]
        . G.insNode wde
    return $ EdgeBranch wde

setEdgeGeometry :: Edge -> LineString -> ModelAction ()
setEdgeGeometry (Edge (e, _)) =
    modifyGraph . modifyNodeLabel e . const . EdgeLabel

setBranchGeometry :: EdgeBranch -> LineString -> ModelAction ()
setBranchGeometry (EdgeBranch (b, _)) =
    modifyGraph . modifyNodeLabel b . const . EdgeBranchLabel

rerouteEdge :: Edge -> LineString -> ModelAction ()
rerouteEdge ee@(Edge (e, _)) geom = do
    m <- S.get
    let [uxy, vxy] = map (nodeXY m . endNode m) $ edgeEnds m ee
    modifyGraph . modifyNodeLabel e . const . EdgeLabel $ snapGeometry_ uxy vxy geom

rerouteBranch :: EdgeBranch -> LineString -> ModelAction ()
rerouteBranch udb@(EdgeBranch (ud, _)) geom' = do
    m <- S.get
    let dxy = diamondXY m $ branchDiamond m udb
        uxy = nodeXY m $ branchNode m udb
    modifyGraph . modifyNodeLabel ud . const . EdgeBranchLabel $ snapGeometry_ dxy uxy geom'

makeBranchFromEdge :: EdgeDiamond -> EdgeEnd -> LineString -> ModelAction ()
makeBranchFromEdge d'd@(EdgeDiamond (d', _)) feend@(EdgeEnd (fe, _)) geom = do
    m <- S.get
    let Edge (e, _) = endEdge m feend
        EdgeEnd (me, _) = endOther m feend
        Node (_, NodeLabel fxy) = endNode m feend
        d'xy = diamondXY m d'd
    modifyGraph
        $ G.insEdge (fe, d', ())
        . G.delNodes [e, me]
        . modifyNodeLabel fe (const . EdgeBranchLabel $ snapGeometry_ d'xy fxy geom)

makeEdgeFromBranch :: EdgeBranch -> Node -> LineString -> ModelAction Edge
makeEdgeFromBranch vdb@(EdgeBranch (vd, _)) (Node (u', NodeLabel u'xy)) geom = do
    m <- S.get
    let [e', u'e'] = G.newNodes 2 $ modelGraph m
        vxy = nodeXY m $ branchNode m vdb
        dd@(EdgeDiamond (d, _)) = branchDiamond m vdb
        e'e = (e', EdgeLabel $ snapGeometry_ u'xy vxy geom)
    modifyGraph
        $ G.insEdges [(vd, e', ()), (u'e', u', ()), (u'e', e', ())]
        . G.delEdge (vd, d)
        . G.insNodes [e'e, (u'e', EdgeEndLabel Forward)]
        . modifyNodeLabel vd (const $ EdgeEndLabel Reverse)
    normalizeDiamond dd
    return $ Edge e'e

reconnectEdgeEnd :: EdgeEnd -> Node -> LineString -> ModelAction ()
reconnectEdgeEnd veend@(EdgeEnd (ve, EdgeEndLabel dir)) (Node (v', NodeLabel v'xy)) geom = do
    m <- S.get
    let Node (v, _) = endNode m veend
        Edge (e, _) = endEdge m veend
        uxy = nodeXY m . endNode m $ endOther m veend
    S.when (v /= v') $
        modifyGraph
        $ G.insEdge (ve, v', ())
        . G.delEdge (ve, v)
        . (modifyNodeLabel e . const . EdgeLabel
           $ liftG (dirReverse dir) (snapGeometry_ uxy v'xy) geom)

reconnectBranchNode :: EdgeBranch -> Node -> LineString -> ModelAction ()
reconnectBranchNode wdb@(EdgeBranch (wd, _)) (Node (w', NodeLabel w'xy)) geom = do
    m <- S.get
    let Node (w, _) = branchNode m wdb
        dxy = diamondXY m $ branchDiamond m wdb
    S.when (w /= w') $
        modifyGraph
        $ G.insEdge (wd, w', ())
        . G.delEdge (wd, w)
        . (modifyNodeLabel wd . const . EdgeBranchLabel $ snapGeometry_ dxy w'xy geom)

reconnectBranchDiamond :: EdgeDiamond -> EdgeBranch -> LineString -> ModelAction ()
reconnectBranchDiamond d'd@(EdgeDiamond (d', _)) wdb@(EdgeBranch (wd, _)) geom = do
    m <- S.get
    let dd@(EdgeDiamond (d, _)) = branchDiamond m wdb
        wxy = nodeXY m $ branchNode m wdb
        d'xy = diamondXY m d'd
    S.when (d /= d') $
        modifyGraph
        $ G.insEdge (wd, d', ())
        . G.delEdge (wd, d)
        . (modifyNodeLabel wd . const . EdgeBranchLabel $ snapGeometry_ d'xy wxy geom)
    normalizeDiamond dd

moveEdgeDiamond :: EdgeDiamond -> Point -> ModelAction ()
moveEdgeDiamond d xy' = do
    S.modify $ mapElements idE idE idE fBranch fDiamond
    where fDiamond (d', _)
              | d === d'  = Just $ EdgeDiamondLabel xy'
              | otherwise = Nothing
          fBranch (EdgeBranch (_, EdgeBranchLabel udgeom),
                   (Node (_, NodeLabel uxy), d'))
              | d === d'  = Just . EdgeBranchLabel $ snapGeometry_ xy' uxy udgeom
              | otherwise = Nothing

deleteEdge :: Edge -> ModelAction ()
deleteEdge e = do
    ends <- S.gets edgeEnds <*> pure e
    modifyGraph $ G.delNodes (map fst $ toElement e : map toElement ends)

deleteEdgeDiamond :: EdgeDiamond -> ModelAction ()
deleteEdgeDiamond d = do
    branches <- S.gets diamondBranches <*> pure d
    modifyGraph $ G.delNodes (map fst $ toElement d : map toElement branches)

deleteBranch :: EdgeBranch -> ModelAction ()
deleteBranch udb@(EdgeBranch (ud, _)) = do
    d <- S.gets branchDiamond <*> pure udb
    modifyGraph $ G.delNode ud
    normalizeDiamond d

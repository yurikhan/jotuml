{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Model
       (Point, Direction (..), dirReverse, (^$^),
        LineString, (^.^), moveGeometry, normalizeBend, moveBend, addBend,
        Distance (..),
        Model, empty, star,
        modelNodes, modelEdges, modelEdgeEnds,
        modelEdgeDiamonds, modelEdgeBranches,
        Element, ElementLabel (..), elementId,
        PointElement (..), LinearElement (..), EdgeElement (..),
        Node (), nodeXY, nodeSize, nodeText,
        nodeBoundary, snapToNode, nodeEnds, nodeEdges,
        nodeDiamonds, nodeBranches, nodeRadius,
        Edge (), edgeBends, edgeGeometry, edgeEnds, edgeNodes,
        EdgeDiamond (), diamondXY, snapToDiamond, diamondBranches, diamondNodes,
        EdgeEnd (), endDirection, endXY, endNode, endEdge, endOther,
        EdgeBranch (), branchBends, branchGeometry, branchStartXY, branchEndXY,
        branchNode, branchDiamond, branchOthers,
        HitTest (..), hitTest,
        ModelAction, idModel,
        addNode, moveNode, deleteNode,
        addEdge, addBranch,
        splitEdge, splitAtBend,
        makeBranchFromEdge, makeEdgeFromBranch,
        rerouteEdge, rerouteBranch,
        reconnectEdgeEnd, reconnectBranchNode, reconnectBranchDiamond,
        moveEdgeDiamond, deleteEdge, detachBranch, deleteBranch) where

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
import Data.VectorSpace ((^+^), (^-^), (<.>), (*^))
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
type Size = (Double, Double)
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

(^.^) :: Direction -> (LineString -> LineString) -> LineString -> LineString
infix 8 ^.^
Forward ^.^ f = f
Reverse ^.^ f = reverse . f . reverse

segments :: LineString -> [Segment]
segments ls = ls `zip` tail ls

unsegments :: [Segment] -> LineString
unsegments ss = fst (head ss) : map snd ss

moveGeometry :: Point -> LineString -> LineString
moveGeometry dxy = map (^+^ dxy)

geometryBends :: LineString -> LineString
geometryBends = tail . init

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
mapNodes f g
    | G.isEmpty g = g
    | otherwise = let ((ins, n, l, outs), g') = G.matchAny g
                  in (ins, n, f (n, l), outs) G.& mapNodes f g'

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
    where points :: [(Point, Size, String)]
          points = [((x, y), (20, 20), show i) | i <- [0..4] :: [G.Node],
                    let x = 300 + 200 * sin (2*pi/5 * fromIntegral i)
                        y = 225 - 200 * cos (2*pi/5 * fromIntegral i)]
          addNodes :: ModelAction [Node]
          addNodes = mapM (\(xy, wh, name) -> addNode xy wh name) points
          addEdges :: [Node] -> ModelAction [(Edge, [EdgeEnd])]
          addEdges ns = S.zipWithM addEdge' ns (drop 2 $ cycle ns)
          addEdge' :: Node -> Node -> ModelAction (Edge, [EdgeEnd])
          addEdge' u v = addEdge Forward u v []


type Graph = G.Gr ElementLabel ()

data ElementLabel = NodeLabel Point Size String -- center of rectangle, text
                  | EdgeLabel LineString -- bends from Forward to Reverse end
                  | EdgeDiamondLabel Point -- center of diamond
                  | EdgeEndLabel Direction
                  | EdgeBranchLabel LineString -- bends from diamond to node
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
    getBends :: Model -> a -> LineString
    getGeometry :: Model -> a -> LineString
    rerouteBends :: a -> LineString -> ModelAction ()
    rerouteGeometry :: a -> LineString -> ModelAction ()
    rerouteGeometry a geom = rerouteBends a $ geometryBends geom
instance LinearElement Edge where
    getBends = edgeBends
    getGeometry = edgeGeometry
    rerouteBends = rerouteEdge
instance LinearElement EdgeBranch where
    getBends = branchBends
    getGeometry = branchGeometry
    rerouteBends = rerouteBranch

class EdgeElement a where delete :: a -> ModelAction ()
instance EdgeElement Edge where delete = deleteEdge
instance EdgeElement EdgeDiamond where delete = deleteEdgeDiamond

(===) :: ElementClass elt => elt -> elt -> Bool
(===) = (==) `on` elementId

isNode :: Element -> Bool
isNode (_, NodeLabel _ _ _) = True
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
nodeXY _ (Node (_, NodeLabel xy _ _)) = xy

nodeSize :: Model -> Node -> Size
nodeSize _ (Node (_, NodeLabel _ wh _)) = wh

nodeText :: Model -> Node -> String
nodeText _ (Node (_, NodeLabel _ _ s)) = s

nodeBoundary :: Model -> Node -> LineString
nodeBoundary _ (Node (_, NodeLabel (x, y) (w, h) _)) =
    [(x - w/2, y - w/2),
     (x - w/2, y + w/2),
     (x + w/2, y + w/2),
     (x + w/2, y - w/2),
     (x - w/2, y - w/2)]

snapToNode :: Model -> Node -> LineString -> LineString
snapToNode m n geom@(xy':_) =
    projection xy' (nodeBoundary m n) : geom

nodeEnds :: Model -> Node -> [EdgeEnd]
nodeEnds m = map EdgeEnd . filter isEdgeEnd . elementNeighbors m

nodeEdges :: Model -> Node -> [Edge]
nodeEdges m = map (endEdge m) . nodeEnds m

nodeBranches :: Model -> Node -> [EdgeBranch]
nodeBranches m = map EdgeBranch . filter isEdgeBranch . elementNeighbors m

nodeDiamonds :: Model -> Node -> [EdgeDiamond]
nodeDiamonds m = map (branchDiamond m) . nodeBranches m


edgeBends :: Model -> Edge -> LineString
edgeBends _ (Edge (_, EdgeLabel bends)) = bends

edgeGeometry :: Model -> Edge -> LineString
edgeGeometry m e = snapToNode m u
                   . tail
                   . (Reverse ^.^ snapToNode m v)
                   $ uxy : bs
    where [u, v] = edgeNodes m e
          uxy = nodeXY m u
          bs = edgeBends m e

edgeEnds :: Model -> Edge -> [EdgeEnd]
edgeEnds m = map EdgeEnd . Ext.sortWith snd . filter isEdgeEnd . elementNeighbors m

edgeEnd :: Model -> Direction -> Edge -> EdgeEnd
edgeEnd m dir = head . filter ((== dir) . endDirection m) . edgeEnds m

edgeNodes :: Model -> Edge -> [Node]
edgeNodes m = map (endNode m) . edgeEnds m


diamondXY :: Model -> EdgeDiamond -> Point
diamondXY _ (EdgeDiamond (_, EdgeDiamondLabel xy)) = xy

snapToDiamond :: Model -> EdgeDiamond -> LineString -> LineString
snapToDiamond m d g@(xy:_) =
    let dxy = diamondXY m $ d
        (dx, dy) = xy ^-^ dxy
        r = nodeRadius
        r' = abs dx + abs dy
        k = if r' > 0 then r / r' else 0
        axy = dxy ^+^ k *^ (dx, dy)
    in axy : g

diamondBranches :: Model -> EdgeDiamond -> [EdgeBranch]
diamondBranches m = map EdgeBranch . filter isEdgeBranch . elementNeighbors m

diamondNodes :: Model -> EdgeDiamond -> [Node]
diamondNodes m = map (branchNode m) . diamondBranches m


endDirection :: Model -> EdgeEnd -> Direction
endDirection _ (EdgeEnd (_, EdgeEndLabel dir)) = dir

endXY :: Model -> EdgeEnd -> Point
endXY m end = geomEnd dir $ edgeGeometry m e
    where dir = endDirection m end
          e = endEdge m end

endNode :: Model -> EdgeEnd -> Node
endNode m = Node . head . filter isNode . elementNeighbors m

endEdge :: Model -> EdgeEnd -> Edge
endEdge m = Edge . head . filter isEdge . elementNeighbors m

endOther :: Model -> EdgeEnd -> EdgeEnd
endOther m end = head . L.deleteBy (===) end . edgeEnds m $ endEdge m end


branchBends :: Model -> EdgeBranch -> LineString
branchBends _ (EdgeBranch (_, EdgeBranchLabel bends)) = bends

branchGeometry :: Model -> EdgeBranch -> LineString
branchGeometry m b = (Reverse ^.^ snapToNode m n)
                     . init
                     . snapToDiamond m d
                     $ bs ++ [nxy]
    where n = branchNode m b
          d = branchDiamond m b
          bs = branchBends m b
          nxy = nodeXY m n

branchStartXY :: Model -> EdgeBranch -> Point
branchStartXY m b = head $ branchGeometry m b

branchEndXY :: Model -> EdgeBranch -> Point
branchEndXY m b = last $ branchGeometry m b

branchNode :: Model -> EdgeBranch -> Node
branchNode m = Node . head . filter isNode . elementNeighbors m

branchDiamond :: Model -> EdgeBranch -> EdgeDiamond
branchDiamond m = EdgeDiamond . head . filter isEdgeDiamond . elementNeighbors m

branchOthers :: Model -> EdgeBranch -> [EdgeBranch]
branchOthers m b = L.deleteBy (===) b . diamondBranches m $ branchDiamond m b


idE :: ElementClass elt => (elt, context) -> Maybe ElementLabel
idE = const Nothing

-- Transform each kind of graph elements’ labels with a suitable function.
-- Each function is passed the neighborhood of the element being tranformed.
-- For fEdge, the (end, node) pairs are ordered Forward first, Reverse last.
mapElements :: ((Node, ()) -> Maybe ElementLabel)
               -> ((EdgeEnd, (Node, Edge)) -> Maybe ElementLabel)
               -> ((Edge, [(EdgeEnd, Node)]) -> Maybe ElementLabel)
               -> ((EdgeBranch, (Node, EdgeDiamond)) -> Maybe ElementLabel)
               -> ((EdgeDiamond, [(EdgeBranch, Node)]) -> Maybe ElementLabel)
               -> Model -> Model
mapElements fNode fEnd fEdge fBranch fDiamond m@(Model g) = Model $ mapNodes f g
    where f elt
              | isNode elt = label' $ fNode (Node elt, ())
              | isEdge elt = let
                  edge = Edge elt
                  endsNodes = map (id &&& endNode m)
                              $ edgeEnds m edge
                  in label' $ fEdge (edge, endsNodes)
              | isEdgeEnd elt = let
                  end = EdgeEnd elt
                  node = endNode m end
                  edge = endEdge m end
                  in label' $ fEnd (end, (node, edge))
              | isEdgeDiamond elt = let
                  diamond = EdgeDiamond elt
                  branchesNodes = map (id &&& branchNode m)
                                  $ diamondBranches m diamond
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
             | OnEdgeBend      Edge Int -- 1 to n-2; see also OnEdgeEnd
             | OnEdgeDiamond   EdgeDiamond
             | OnEdgeEnd       EdgeEnd
             | OnBranchStart   EdgeBranch
             | OnBranchSegment EdgeBranch Int -- 0 to n-2 outwards
             | OnBranchBend    EdgeBranch Int -- 1 to n-2 outwards
             | OnBranchEnd     EdgeBranch
             | Nowhere
             deriving (Show)

distanceTo :: Model -> Point -> HitTest -> (Int, Maybe Double)
distanceTo m xy ht = case ht of
    OnNode n            | within xy (nodeXY m n) (nodeSize m n) -> (0, Just 0)
                        | otherwise -> (0, Nothing)
    OnEdgeDiamond d     -> 0 ^+ diamondXY m d
    OnEdgeEnd e         -> 1 ^+ endXY m e
    OnBranchStart b     -> 1 ^+ branchStartXY m b
    OnBranchEnd b       -> 1 ^+ branchEndXY m b
    OnEdgeBend e i      -> 2 ^+ edgeBends m e !! (i-1)
    OnBranchBend b i    -> 2 ^+ branchBends m b !! (i-1)
    OnEdgeSegment e i   -> 3 ^+ segments (edgeGeometry m e) !! i
    OnBranchSegment b i -> 3 ^+ segments (branchGeometry m b) !! i
    Nowhere             -> (9, Just 0)
    where rank ^+ geom
              | distance xy geom < nodeRadius = (rank, Just $ distance xy geom)
              | otherwise = (rank, Nothing)
          infix 0 ^+
          within (x', y') (x, y) (w, h) = abs (x' - x) < w / 2
                                          && abs (y' - y) < h / 2

hitTest :: Model -> Point -> HitTest
hitTest m xy = fst
               . L.minimumBy (comparing snd)
               . filter (Mb.isJust . snd . snd)
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


addNode :: Point -> Size -> String -> ModelAction Node
addNode xy wh name = do
    [n] <- G.newNodes 1 <$> getGraph
    let ne = (n, NodeLabel xy wh name)
    modifyGraph $ G.insNode ne
    return $ Node ne

-- Move NODE to POINT. Also move its self-edges (if any).
moveNode :: Node -> Point -> ModelAction ()
moveNode n@(Node (_, NodeLabel xy wh name)) xy' =
    S.modify $ mapElements fNode idE fEdge idE idE
    where fNode (n', _) | n === n' = Just $ NodeLabel xy' wh name
                        | otherwise = Nothing
          fEdge (Edge (_, EdgeLabel bends), [(_, n'), (_, n'')])
              | n === n' && n === n'' = Just . EdgeLabel $ moveGeometry dxy bends
              | otherwise = Nothing
          dxy = xy' ^-^ xy

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


-- If a DIAMOND has only two branches, turn it into an edge.
normalizeDiamond :: EdgeDiamond -> ModelAction ()
normalizeDiamond dd@(EdgeDiamond (d, EdgeDiamondLabel xy)) = do
    m <- S.get
    let branches = diamondBranches m dd
    case branches of
        [udb@(EdgeBranch (ud, EdgeBranchLabel udbends)),
         vdb@(EdgeBranch (vd, EdgeBranchLabel vdbends))] ->
            modifyGraph
            $ modifyNodeLabel  d (const $ EdgeLabel bends')
            . modifyNodeLabel ud (const $ EdgeEndLabel Forward)
            . modifyNodeLabel vd (const $ EdgeEndLabel Reverse)
            where
                bends' = geometryBends
                         $ normalizeBend (length before)
                         $ before ++ xy : after
                before = branchEndXY m udb : reverse udbends
                after = vdbends ++ [branchEndXY m vdb]
        _ -> return ()

addEdge :: Direction -> Node -> Node -> LineString -> ModelAction (Edge, [EdgeEnd])
addEdge Reverse v u bends = addEdge Forward u v (reverse bends)
addEdge Forward (Node (u, _)) (Node (v, _)) bends = do
    [e, ue, ve] <- G.newNodes 3 <$> getGraph
    let ee = (e, EdgeLabel bends)
        uee = (ue, EdgeEndLabel Forward)
        vee = (ve, EdgeEndLabel Reverse)
    modifyGraph
        $ G.insEdges [(ue, u, ()), (ue, e, ()), (ve, v, ()), (ve, e, ())]
        . G.insNodes [ee, uee, vee]
    return (Edge ee, map EdgeEnd [uee, vee])

-- Turn an EDGE into a diamond near the given POINT and perform an ACTION on it,
-- passing the specified PARAMETER and BENDS.
splitEdge :: Edge -> Point -> (EdgeDiamond -> a -> LineString -> ModelAction r)
             -> a -> LineString -> ModelAction r
splitEdge ee@(Edge (e, _)) xy modifyDiamond a bends = do
    m <- S.get
    let geom = getGeometry m ee
        dxy = projection xy geom
        [EdgeEnd (ue, _), EdgeEnd (ve, _)] = edgeEnds m ee
        (uegeom, vegeom) = splitGeometry dxy geom
        d = EdgeDiamond (e, EdgeDiamondLabel dxy)
    modifyGraph
        $ modifyNodeLabel  e (const $ EdgeDiamondLabel dxy)
        . modifyNodeLabel ue (const $ EdgeBranchLabel $ geometryBends uegeom)
        . modifyNodeLabel ve (const $ EdgeBranchLabel $ geometryBends vegeom)
    modifyDiamond d a bends

splitAtBend :: Edge -> Int -> (EdgeDiamond -> a -> LineString -> ModelAction r)
              -> a -> LineString -> ModelAction r
splitAtBend e@(Edge (_, EdgeLabel ebends)) i =
    splitEdge e $ ebends !! (i-1)

-- Add a branch from a DIAMOND to a NODE through BENDS.
addBranch :: EdgeDiamond -> Node -> LineString -> ModelAction EdgeBranch
addBranch (EdgeDiamond (d, _)) (Node (w, _)) wdbends = do
    [wd] <- G.newNodes 1 <$> getGraph
    let wde = (wd, EdgeBranchLabel wdbends)
    modifyGraph
        $ G.insEdges [(wd, w, ()), (wd, d, ())]
        . G.insNode wde
    return $ EdgeBranch wde

rerouteEdge :: Edge -> LineString -> ModelAction ()
rerouteEdge (Edge (e, _)) =
    modifyGraph . modifyNodeLabel e . const . EdgeLabel

rerouteBranch :: EdgeBranch -> LineString -> ModelAction ()
rerouteBranch (EdgeBranch (b, _)) =
    modifyGraph . modifyNodeLabel b . const . EdgeBranchLabel

-- Given a fixed END, detach the other end from its node and reattach to a
-- DIAMOND, making it a branch. Route the new branch through BENDS.
makeBranchFromEdge :: EdgeDiamond -> EdgeEnd -> LineString -> ModelAction ()
makeBranchFromEdge (EdgeDiamond (d', _)) feend@(EdgeEnd (fe, _)) bends = do
    m <- S.get
    let Edge (e, _) = endEdge m feend
        EdgeEnd (me, _) = endOther m feend
    modifyGraph
        $ G.insEdge (fe, d', ())
        . G.delNodes [e, me]
        . modifyNodeLabel fe (const $ EdgeBranchLabel bends)

-- Turn a BRANCH into a new edge by detaching it from the diamond and
-- reattaching to a NODE. The new edge is routed through BENDS.
makeEdgeFromBranch :: EdgeBranch -> Node -> LineString -> ModelAction Edge
makeEdgeFromBranch vdb@(EdgeBranch (vd, _)) (Node (u', _)) bends = do
    m <- S.get
    let [e', u'e'] = G.newNodes 2 $ modelGraph m
        dd@(EdgeDiamond (d, _)) = branchDiamond m vdb
        e'e = (e', EdgeLabel bends)
    modifyGraph
        $ G.insEdges [(vd, e', ()), (u'e', u', ()), (u'e', e', ())]
        . G.delEdge (vd, d)
        . G.insNodes [e'e, (u'e', EdgeEndLabel Forward)]
        . modifyNodeLabel vd (const $ EdgeEndLabel Reverse)
    normalizeDiamond dd
    return $ Edge e'e

-- Detach edge END from its node and reattach to another NODE.
-- Reroute the edge through BENDS.
reconnectEdgeEnd :: EdgeEnd -> Node -> LineString -> ModelAction ()
reconnectEdgeEnd meend@(EdgeEnd (me, _)) (Node (mn', _)) bends = do
    m <- S.get
    let Node (mn, _) = endNode m meend
        Edge (e, _) = endEdge m meend
    S.when (mn /= mn') $
        modifyGraph
        $ G.insEdge (me, mn', ())
        . G.delEdge (me, mn)
        . (modifyNodeLabel e . const $ EdgeLabel bends)

-- Disconnect the BRANCH from its node and reconnect to another NODE.
-- Reroute through BENDS.
reconnectBranchNode :: EdgeBranch -> Node -> LineString -> ModelAction ()
reconnectBranchNode wdb@(EdgeBranch (wd, _)) (Node (w', _)) bends = do
    Node (w, _) <- S.gets branchNode <*> pure wdb
    S.when (w /= w') $
        modifyGraph
        $ G.insEdge (wd, w', ())
        . G.delEdge (wd, w)
        . (modifyNodeLabel wd . const $ EdgeBranchLabel bends)

-- Disconnect the BRANCH from its diamond and reconnect to another DIAMOND.
-- Reroute through BENDS.
reconnectBranchDiamond :: EdgeDiamond
                          -> EdgeBranch
                          -> LineString
                          -> ModelAction ()
reconnectBranchDiamond (EdgeDiamond (d', _)) wdb@(EdgeBranch (wd, _)) bends = do
    dd@(EdgeDiamond (d, _)) <- S.gets branchDiamond <*> pure wdb
    S.when (d /= d') $
        modifyGraph
        $ G.insEdge (wd, d', ())
        . G.delEdge (wd, d)
        . (modifyNodeLabel wd . const $ EdgeBranchLabel bends)
    normalizeDiamond dd

-- Move a DIAMOND to another POINT.
moveEdgeDiamond :: EdgeDiamond -> Point -> ModelAction ()
moveEdgeDiamond (EdgeDiamond (d, _)) = do
    modifyGraph . modifyNodeLabel d . const . EdgeDiamondLabel

deleteEdge :: Edge -> ModelAction ()
deleteEdge e = do
    ends <- S.gets edgeEnds <*> pure e
    modifyGraph $ G.delNodes (map fst $ toElement e : map toElement ends)

deleteEdgeDiamond :: EdgeDiamond -> ModelAction ()
deleteEdgeDiamond d = do
    branches <- S.gets diamondBranches <*> pure d
    modifyGraph $ G.delNodes (map fst $ toElement d : map toElement branches)

-- Temporarily delete a BRANCH. The diamond is not normalized.
detachBranch :: EdgeBranch -> ModelAction ()
detachBranch udb@(EdgeBranch (ud, _)) = do
    d <- S.gets branchDiamond <*> pure udb
    modifyGraph $ G.delNode ud

-- Delete a BRANCH and if it leaves only two branches then
-- turn the corresponding diamond into an edge.
deleteBranch :: EdgeBranch -> ModelAction ()
deleteBranch udb@(EdgeBranch (ud, _)) = do
    d <- S.gets branchDiamond <*> pure udb
    modifyGraph $ G.delNode ud
    normalizeDiamond d

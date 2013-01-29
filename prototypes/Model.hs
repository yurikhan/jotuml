{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Model
       (Point, Direction (..), dirReverse, (^$^),
        LineString, makeGeometry, liftG, moveGeometry, snapGeometry,
        moveBend, addBend,
        Distance (..),
        Model, empty, star, modelNodes, modelEdges, modelEdgeEnds,
        Element, ElementLabel (..),
        Node (), nodeXY, nodeEnds, nodeEdges, nodeRadius, nodeMove,
        Edge (), edgeIsHyper, edgeDiamondXY, edgeDiamondMove, edgeGeometry, edgeEnds, edgeNodes,
        EdgeEnd (), endIsHyper, endDirection, endXY, endGeometry, endNode, endEdge,
        endOthers, endOther,
        HitTest (..), hitTest,
        ModelAction, idModel,
        addNode, moveNode, deleteNode,
        addEdge, addEdgeEnd, addEdgeEndAtBend,
        splitEdge, detachEdgeEnd,
        makeBranchFromEdge, makeBranchFromEdgeAtBend, makeEdgeFromBranch,
        setEdgeGeometry, setEdgeEndGeometry,
        rerouteEdge, rerouteEdgeEnd,
        reconnectEdge, reconnectEdgeEnd, reconnectBranch, reconnectBranchAtBend,
        moveEdgeDiamond, deleteEdge) where

import Control.Arrow ((&&&), (***))
import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad.State.Lazy as S
import qualified Data.Graph.Inductive as G
import qualified Data.List as L
import qualified Data.Maybe as Mb
import qualified Data.VectorSpace as V
import Data.VectorSpace ((^+^), (^-^), (<.>))
import qualified GHC.Exts as Ext

import Debug.Trace

trace' :: Show a => a -> a
trace' x = traceShow x x

{- Auxiliary: Geometry -}

nodeRadius :: Double
nodeRadius = 10

data Direction = Forward | Reverse deriving (Eq, Show)

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

moveBend :: Int -> Point -> LineString -> LineString
moveBend i xy g = let (before, _ : after) = splitAt i g
                  in before ++ xy : after

addBend :: Int -> Point -> LineString -> LineString
addBend i xy g = let (before, after) = splitAt (i + 1) g
                     in before ++ xy : after


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
star = execState (addEdges =<< addNodes) empty
    where points :: [Point]
          points = [(x, y) | i <- [0..4] :: [G.Node],
                    let x = 300 + 200 * sin (2*pi/5 * fromIntegral i)
                        y = 225 - 200 * cos (2*pi/5 * fromIntegral i)]
          addNodes :: ModelAction [Node]
          addNodes = mapM addNode points
          addEdges :: [Node] -> ModelAction [(Edge, [EdgeEnd])]
          addEdges ns = zipWithM addEdge' ns (drop 2 $ cycle ns)
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
                  | HyperEdgeLabel Point -- center of diamond
                  | EdgeEndLabel Direction
                  | HyperEdgeEndLabel LineString -- from diamond to node
                  deriving (Show)

type Element = G.LNode ElementLabel
newtype Node = Node Element deriving (Show)
newtype Edge = Edge Element deriving (Show)
newtype EdgeEnd = EdgeEnd Element deriving (Show)

class ElementClass elt where
    toElement :: elt -> Element
instance ElementClass Element where
    toElement = id
instance ElementClass Node where
    toElement (Node e) = e
instance ElementClass Edge where
    toElement (Edge e) = e
instance ElementClass EdgeEnd where
    toElement (EdgeEnd e) = e

instance Distance Edge where
    projection xy (Edge (_, EdgeLabel geom)) = projection xy geom
    projection _ (Edge (_, HyperEdgeLabel exy)) = exy

sameElement :: ElementClass elt => elt -> elt -> Bool
sameElement e e' = fst (toElement e) == fst (toElement e')

isNode :: Element -> Bool
isNode (_, NodeLabel _) = True
isNode _ = False

isEdge :: Element -> Bool
isEdge (_, EdgeLabel _) = True
isEdge (_, HyperEdgeLabel _) = True
isEdge _ = False

isEdgeEnd :: Element -> Bool
isEdgeEnd (_, EdgeEndLabel _) = True
isEdgeEnd (_, HyperEdgeEndLabel _) = True
isEdgeEnd _ = False


elementNeighbors :: Model -> Element -> [Element]
elementNeighbors (Model g) (n, _) = labNeighbors g n

modelElements :: Model -> [Element]
modelElements (Model g) = G.labNodes g

modelNodes :: Model -> [Node]
modelNodes = map Node . filter isNode . modelElements

modelEdges :: Model -> [Edge]
modelEdges = map Edge . filter isEdge . modelElements

modelEdgeEnds :: Model -> [EdgeEnd]
modelEdgeEnds = map EdgeEnd . filter isEdgeEnd . modelElements


nodeXY :: Model -> Node -> Point
nodeXY _ (Node (_, NodeLabel xy)) = xy

nodeEnds :: Model -> Node -> [EdgeEnd]
nodeEnds m (Node n) = map EdgeEnd . filter isEdgeEnd $ elementNeighbors m n

nodeEdges :: Model -> Node -> [Edge]
nodeEdges m n = map (endEdge m) $ nodeEnds m n

nodeMove :: Node -> Point -> Node
nodeMove (Node (n, NodeLabel _)) xy' = Node (n, NodeLabel xy')


edgeIsHyper :: Model -> Edge -> Bool
edgeIsHyper _ (Edge (_, HyperEdgeLabel _)) = True
edgeIsHyper _ _ = False

edgeGeometry :: Model -> Edge -> LineString
edgeGeometry _ (Edge (_, EdgeLabel geom)) = geom

edgeDiamondXY :: Model -> Edge -> Point
edgeDiamondXY _ (Edge (_, HyperEdgeLabel xy)) = xy

edgeDiamondMove :: Edge -> Point -> Edge
edgeDiamondMove (Edge (e, HyperEdgeLabel _)) xy' = Edge (e, HyperEdgeLabel xy')

edgeEndNodeXY :: Model -> Direction -> Edge -> Point
edgeEndNodeXY m dir e = nodeXY m . endNode m $ edgeEnd m dir e

edgeEnds :: Model -> Edge -> [EdgeEnd]
edgeEnds m (Edge e) = map EdgeEnd . filter isEdgeEnd $ elementNeighbors m e

edgeEnd :: Model -> Direction -> Edge -> EdgeEnd
edgeEnd m dir e = head . filter ((== dir) . endDirection m) $ edgeEnds m e

edgeNodes :: Model -> Edge -> [Node]
edgeNodes m e = map (endNode m) $ edgeEnds m e


endIsHyper :: Model -> EdgeEnd -> Bool
endIsHyper _ (EdgeEnd (_, HyperEdgeEndLabel _)) = True
endIsHyper _ _ = False

endDirection :: Model -> EdgeEnd -> Direction
endDirection m (EdgeEnd (_, EdgeEndLabel dir)) = dir

endXY :: Model -> EdgeEnd -> Point
endXY m end@(EdgeEnd (_, EdgeEndLabel _)) =
    geomEnd (endDirection m end) . edgeGeometry m $ endEdge m end
endXY m end@(EdgeEnd (_, HyperEdgeEndLabel geom)) = last geom

endGeometry :: Model -> EdgeEnd -> LineString
endGeometry m end@(EdgeEnd (_, HyperEdgeEndLabel geom)) = geom

endNode :: Model -> EdgeEnd -> Node
endNode m (EdgeEnd e) = Node . head . filter isNode $ elementNeighbors m e

endEdge :: Model -> EdgeEnd -> Edge
endEdge m (EdgeEnd e) = Edge . head . filter isEdge $ elementNeighbors m e

endOthers :: Model -> EdgeEnd -> [EdgeEnd]
endOthers m end = L.deleteBy sameElement end $ edgeEnds m $ endEdge m end

endOther :: Model -> EdgeEnd -> EdgeEnd
endOther m end = head $ endOthers m end


mapElements :: (Node -> ElementLabel) -- labeled node -> new label
               -> ((EdgeEnd, Node, Edge) -> ElementLabel) -- end, node, edge -> new label
               -> ((Edge, [EdgeEnd], [Node]) -> ElementLabel) -- edge, ends, nodes -> new label
               -> Model -> Model
mapElements fNode fEnd fEdge m@(Model g) = Model $ mapNodes f g
    where f elt
              | isNode elt = fNode $ Node elt
              | isEdge elt = let
                  ends = edgeEnds m edge
                  nodes = edgeNodes m edge
                  edge = Edge elt
                  in fEdge (edge, ends, nodes)
              | isEdgeEnd elt = let
                  node = endNode m end'
                  edge = endEdge m end'
                  end' = EdgeEnd elt
                  in fEnd (end', node, edge)


data HitTest = OnNode          Node
             | OnEdgeSegment   Edge Int -- 0 to n-2 where n = length geom
             | OnEdgeBend      Edge Int -- 1 to n-2; 0th and (n-1)th report OnEdgeEnd
             | OnEdgeDiamond   Edge
             | OnEdgeEnd       EdgeEnd
             | OnBranchStart   EdgeEnd
             | OnBranchSegment EdgeEnd Int -- 0 to n-2 outwards
             | OnBranchBend    EdgeEnd Int -- 1 to n-2 outwards
             | OnBranchEnd     EdgeEnd
             | Nowhere
             deriving (Show)

distanceTo :: Model -> Point -> HitTest -> (Int, Double)
distanceTo _ xy (OnNode        (Node (_, NodeLabel xy')))      = (0, distance xy xy')
distanceTo _ xy (OnEdgeDiamond (Edge (_, HyperEdgeLabel xy'))) = (0, distance xy xy')
distanceTo m xy (OnEdgeEnd e@(EdgeEnd (_, EdgeEndLabel _))) =
    (1, distance xy $ endXY m e)
distanceTo _ xy (OnBranchStart (EdgeEnd (_, HyperEdgeEndLabel (xy' : _)))) = 
    (1, distance xy xy')
distanceTo _ xy (OnBranchEnd (EdgeEnd (_, HyperEdgeEndLabel geom))) =
    (1, distance xy $ last geom)
distanceTo _ xy (OnEdgeBend (Edge (_, EdgeLabel geom)) i) = (2, distance xy $ geom !! i)
distanceTo _ xy (OnBranchBend (EdgeEnd (_, HyperEdgeEndLabel geom)) i) =
    (2, distance xy $ geom !! i)
distanceTo _ xy (OnEdgeSegment (Edge (_, EdgeLabel geom)) i) =
    (3, distance xy $ segments geom !! i)
distanceTo _ xy (OnBranchSegment (EdgeEnd (_, HyperEdgeEndLabel geom)) i) =
    (3, distance xy $ segments geom !! i)
distanceTo _ _ Nowhere = (9, 0)

hitTest :: Model -> Point -> HitTest
hitTest m xy = fst . head
               . Ext.sortWith snd
               . filter ((< 10) . snd . snd)
               . map (id &&& distanceTo m xy)
               $ possibilities
    where possibilities =
              Nowhere
              : map OnNode nodes
              ++ concatMap edgePossibilities binaryEdges
              ++ map OnEdgeDiamond naryEdges
              ++ map OnEdgeEnd binaryEnds
              ++ concatMap branchPossibilities naryEnds
          nodes = modelNodes m
          (binaryEdges, naryEdges) = L.partition ((2 ==) . length . edgeEnds m) $ modelEdges m
          (binaryEnds, naryEnds) = L.partition ((1 ==) . length . endOthers m) $ modelEdgeEnds m
          edgePossibilities e =
              map (OnEdgeBend e) [1..n - 2]
              ++ map (OnEdgeSegment e) [0..n - 2]
              where n = length $ edgeGeometry m e
          branchPossibilities end =
              OnBranchStart end
              : OnBranchEnd end
              : map (OnBranchBend end) [1..n - 2]
              ++ map (OnBranchSegment end) [0..n - 2]
              where n = length $ endGeometry m end


type ModelTransition = Model -> Model
type ModelAction r = S.State Model r

idModel :: ModelAction ()
idModel = return ()

modelGraph :: Model -> Graph
modelGraph (Model g) = g

getGraph :: ModelAction Graph
getGraph = S.gets modelGraph

liftModel :: (Graph -> Graph) -> ModelTransition
liftModel f (Model g) = Model $ f g

modifyGraph :: (Graph -> Graph) -> ModelAction ()
modifyGraph = modify . liftModel


addNode :: Point -> ModelAction Node
addNode xy = do
    [nId] <- G.newNodes 1 <$> getGraph
    let n = (nId, NodeLabel xy)
    modifyGraph $ G.insNode n
    return $ Node n

moveNode :: Node -> Point -> ModelAction ()
moveNode (Node (n, _)) xy' = modify $ mapElements fixNode fixEnd fixEdge
    where fixNode (Node (u, uv@(NodeLabel _)))
              | u == n     = NodeLabel xy'
              | otherwise = uv
          fixEdge (e,
                   ends@[EdgeEnd (_, EdgeEndLabel Reverse), EdgeEnd (_, EdgeEndLabel Forward)],
                   nodes) =
              fixEdge (e, reverse ends, reverse nodes)
          fixEdge (Edge (_, ev@(EdgeLabel geom)),
                   [EdgeEnd (_, EdgeEndLabel Forward), EdgeEnd (_, EdgeEndLabel Reverse)],
                   [Node (u, NodeLabel uxy), Node (v, NodeLabel vxy)])
              | u == n && v == n = EdgeLabel $ moveGeometry (xy' ^-^ uxy) $ geom
              | u == n     = EdgeLabel $ snapGeometry_ xy' vxy $ geom
              | v == n     = EdgeLabel $ snapGeometry_ uxy xy' $ geom
              | otherwise = ev
          fixEdge (Edge (_, ev@(HyperEdgeLabel _)), _, _) = ev
          fixEnd (EdgeEnd (_, endv@(EdgeEndLabel _)), _, _) = endv
          fixEnd (EdgeEnd (_, endv@(HyperEdgeEndLabel geom)), Node (u, _), _)
              | u == n     = HyperEdgeEndLabel $ liftG Reverse (snapHead_ xy') geom
              | otherwise = endv

deleteNode :: Node -> ModelAction ()
deleteNode n = do
    m <- get
    let (binaryEdges, naryEdges) = L.partition ((2 ==) . length . edgeEnds m) $ nodeEdges m n
        binaryEnds = concatMap (edgeEnds m) binaryEdges
        naryEnds = filter ((1 <) . length . endOthers m) $ nodeEnds m n
        ternaryEdges = filter ((3 ==) . length . edgeEnds m) naryEdges
    modifyGraph . G.delNodes . map fst
        $ toElement n
        : map toElement binaryEdges
        ++ map toElement (binaryEnds ++ naryEnds)
    mapM_ normalizeEdge ternaryEdges


normalizeEdge :: Edge -> ModelAction ()
normalizeEdge e@(Edge (eId, HyperEdgeLabel xy)) = do
    m <- get
    normalize' $ edgeEnds m e
    where normalize' [EdgeEnd (ue, HyperEdgeEndLabel geom1),
                      EdgeEnd (ve, HyperEdgeEndLabel geom2)] =
              modifyGraph
              $ modifyNodeLabel eId (const $ EdgeLabel geom')
              . modifyNodeLabel ue (const $ EdgeEndLabel Forward)
              . modifyNodeLabel ve (const $ EdgeEndLabel Reverse)
              where
                  geom' = reverse (tail geom1) ++ xy : tail geom2
          normalize' _ = return ()
normalizeEdge _ = return ()

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

splitEdge :: Edge -> Point -> ModelAction Edge
splitEdge ee@(Edge (e, EdgeLabel geom)) xy = do
    m <- get
    let [EdgeEnd (ue, _), EdgeEnd (ve, _)] = map (\d -> edgeEnd m d ee) [Forward, Reverse]
        exy = projection xy geom
        (uegeom, vegeom) = snapHead_ exy *** snapHead_ exy $ splitGeometry exy geom
    modifyGraph
        $ modifyNodeLabel e (const $ HyperEdgeLabel exy)
        . modifyNodeLabel ue (const $ HyperEdgeEndLabel uegeom)
        . modifyNodeLabel ve (const $ HyperEdgeEndLabel vegeom)
    return $ Edge (e, HyperEdgeLabel exy)
splitEdge e _ = return e

addEdgeEnd :: Edge -> Node -> LineString -> ModelAction EdgeEnd
addEdgeEnd ee@(Edge (_, EdgeLabel _)) ww wegeom = do
    ee' <- splitEdge ee (head wegeom)
    addEdgeEnd ee' ww wegeom
addEdgeEnd ee@(Edge (e, HyperEdgeLabel exy)) ww@(Node (w, NodeLabel wxy)) wegeom = do
    m <- get
    let [we] = G.newNodes 1 $ modelGraph m
        wee = (we, HyperEdgeEndLabel $ snapGeometry_ exy wxy wegeom)
    modifyGraph
        $ G.insEdges [(we, w, ()), (we, e, ())]
        . G.insNode wee
    return $ EdgeEnd wee

addEdgeEndAtBend :: Edge -> Int -> Node -> LineString -> ModelAction EdgeEnd
addEdgeEndAtBend e@(Edge(_, EdgeLabel egeom)) i w wegeom =
    addEdgeEnd e w $ snapHead (egeom !! i, 0) wegeom

detachEdgeEnd :: EdgeEnd -> ModelAction ()
detachEdgeEnd (EdgeEnd (we, _)) = modifyGraph $ G.delNode we

setEdgeGeometry :: Edge -> LineString -> ModelAction ()
setEdgeGeometry e@(Edge (eId, EdgeLabel _)) geom' =
    modifyGraph . modifyNodeLabel eId . const $ EdgeLabel geom'

setEdgeEndGeometry :: EdgeEnd -> LineString -> ModelAction ()
setEdgeEndGeometry end@(EdgeEnd (endId, HyperEdgeEndLabel _)) geom' =
    modifyGraph . modifyNodeLabel endId . const $ HyperEdgeEndLabel geom'

rerouteEdge :: Edge -> LineString -> ModelAction ()
rerouteEdge e@(Edge (eId, _)) geom = do
    m <- get
    let [uxy, vxy] = map (\dir -> edgeEndNodeXY m dir e) [Forward, Reverse]
    modifyGraph $ modifyNodeLabel eId $ \(_, EdgeLabel _) ->
        EdgeLabel $ snapGeometry_ uxy vxy geom

rerouteEdgeEnd :: EdgeEnd -> LineString -> ModelAction ()
rerouteEdgeEnd end@(EdgeEnd (endId, _)) geom = do
    m <- get
    let exy = edgeDiamondXY m $ endEdge m end
        uxy = nodeXY m $ endNode m end
    modifyGraph $ modifyNodeLabel endId $ const $ HyperEdgeEndLabel $ snapGeometry_ exy uxy geom

makeBranchFromEdge :: EdgeEnd -> Edge -> LineString -> ModelAction ()
makeBranchFromEdge fe e'@(Edge (_, EdgeLabel _)) geom' = do
    e'' <- splitEdge e' $ head geom'
    makeBranchFromEdge fe e'' geom'
makeBranchFromEdge fe@(EdgeEnd (feId, _)) e'@(Edge (e'Id, HyperEdgeLabel e'xy)) geom = do
    m <- get
    let Edge (eId, _) = endEdge m fe
        EdgeEnd (meId, _) = endOther m fe
        Node (_, NodeLabel fxy) = endNode m fe
    modifyGraph
        $ G.insEdge (feId, e'Id, ())
        . G.delNodes [eId, meId]
        . modifyNodeLabel feId  (const . HyperEdgeEndLabel $ snapGeometry_ e'xy fxy geom)

makeBranchFromEdgeAtBend :: EdgeEnd -> Edge -> Int -> LineString -> ModelAction ()
makeBranchFromEdgeAtBend fe e'@(Edge (_, EdgeLabel egeom)) i geom' =
    makeBranchFromEdge fe e' $ snapHead (egeom !! i, 0) geom'

makeEdgeFromBranch :: EdgeEnd -> Node -> LineString -> ModelAction Edge
makeEdgeFromBranch vee@(EdgeEnd (ve, _)) (Node (u', NodeLabel u'xy)) geom = do
    m <- get
    let [e', u'e'] = G.newNodes 2 $ modelGraph m
        Node (_, NodeLabel vxy) = endNode m vee
        ee@(Edge (e, _)) = endEdge m vee
        e'e = (e', EdgeLabel $ snapGeometry_ u'xy vxy geom)
    modifyGraph
        $ G.insEdges [(ve, e', ()), (u'e', u', ()), (u'e', e', ())]
        . G.delEdge (ve, e)
        . G.insNodes [e'e, (u'e', EdgeEndLabel Forward)]
        . modifyNodeLabel ve (const $ EdgeEndLabel Reverse)
    normalizeEdge ee
    return $ Edge e'e

reconnectEdge :: Edge -> Direction -> Node -> ModelAction ()
reconnectEdge e@(Edge (eId, EdgeLabel geom)) dir (Node (v'Id, NodeLabel v'xy)) = do
    m <- get
    let uxy = edgeEndNodeXY m dir e
        ve@(EdgeEnd (veId, vel@(EdgeEndLabel _))) = edgeEnd m (dirReverse dir) e
        Node (vId, _) = endNode m ve
        [v'eId] = G.newNodes 1 $ modelGraph m
    when (v'Id /= vId) $
        modifyGraph
        $ G.insEdges [(v'eId, v'Id, ()), (v'eId, eId, ())]
        . G.insNode (v'eId, vel)
        . G.delNode veId
        . modifyNodeLabel eId (\(_, EdgeLabel geom') ->
                                EdgeLabel $ liftG dir (snapGeometry_ uxy v'xy) geom')

reconnectEdgeEnd :: EdgeEnd -> Node -> LineString -> ModelAction ()
reconnectEdgeEnd ve@(EdgeEnd (veId, EdgeEndLabel dir)) (Node (v'Id, NodeLabel v'xy)) geom = do
    m <- get
    let Node (vId, _) = endNode m ve
        Edge (eId, EdgeLabel _) = endEdge m ve
        Node (_, NodeLabel uxy) = endNode m $ endOther m ve
    when (vId /= v'Id) $
        modifyGraph
        $ G.insEdge (veId, v'Id, ())
        . G.delEdge (veId, vId)
        . (modifyNodeLabel eId $ const $ EdgeLabel
           $ liftG (dirReverse dir) (snapGeometry_ uxy v'xy) geom)
reconnectEdgeEnd we@(EdgeEnd (weId, HyperEdgeEndLabel _)) (Node (w'Id, NodeLabel w'xy)) geom = do
    m <- get
    let Node (wId, _) = endNode m we
        Edge (_, HyperEdgeLabel exy) = endEdge m we
    when (wId /= w'Id) $
        modifyGraph
        $ G.insEdge (weId, w'Id, ())
        . G.delEdge (weId, wId)
        . (modifyNodeLabel weId $ const $ HyperEdgeEndLabel $ snapGeometry_ exy w'xy geom)

reconnectBranch :: EdgeEnd -> Edge -> LineString -> ModelAction ()
reconnectBranch we e'@(Edge (_, EdgeLabel _)) geom = do
    e'' <- splitEdge e' $ head geom
    reconnectBranch we e'' geom
reconnectBranch we@(EdgeEnd (weId, _)) e'@(Edge (e'Id, HyperEdgeLabel e'xy)) geom = do
    m <- get
    let e@(Edge (eId, _)) = endEdge m we
        Node (_, NodeLabel wxy) = endNode m we
    when (eId /= e'Id) $
        modifyGraph
        $ G.insEdge (weId, e'Id, ())
        . G.delEdge (weId, eId)
        . (modifyNodeLabel weId $ const $ HyperEdgeEndLabel $ snapGeometry_ e'xy wxy geom)
    normalizeEdge e

reconnectBranchAtBend :: EdgeEnd -> Edge -> Int -> LineString -> ModelAction ()
reconnectBranchAtBend we e'@(Edge (_, EdgeLabel e'geom)) i geom =
    reconnectBranch we e' $ snapHead (e'geom !! i, 0) geom

moveEdgeDiamond :: Edge -> Point -> ModelAction ()
moveEdgeDiamond e@(Edge (eId, HyperEdgeLabel exy)) xy' = do
    modify $ mapElements fNode fEnd fEdge
    where fNode (Node (_, nl)) = nl
          fEdge (e'@(Edge (_, e'l)), ends, nodes)
              | sameElement e e' = HyperEdgeLabel xy'
              | otherwise        = e'l
          fEnd (EdgeEnd (_, endv@(HyperEdgeEndLabel geom)),
                Node (_, NodeLabel uxy), Edge (e', _))
              | e' == eId  = HyperEdgeEndLabel $ snapGeometry_ xy' uxy geom
              | otherwise = endv
          fEnd (EdgeEnd (_, endv), _, _) = endv

deleteEdge :: Edge -> ModelAction ()
deleteEdge e = do
    ends <- edgeEnds <$> get <*> pure e
    modifyGraph $ G.delNodes (map fst $ toElement e : map toElement ends)

deleteEdgeEnd :: EdgeEnd -> ModelAction ()
deleteEdgeEnd ue = do
    m <- get
    let e = endEdge m ue
    detachEdgeEnd ue
    normalizeEdge e

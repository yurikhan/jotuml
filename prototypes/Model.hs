module Model
       (Point, Direction (..), dirReverse, LineString, makeGeometry, liftG,
        Model, empty, star, modelNodes, modelEdges, modelEdgeEnds,
        Element, ElementLabel (..),
        nodeXY, nodeEnds, nodeEdges, nodeRadius,
        edgeGeometry, edgeEnds, edgeNodes,
        endXY, endNode, endEdge, endOthers, endOther,
        HitTest, hitTest,
        ModelTransition,
        addNode, moveNode, deleteNode,
        addEdge, rerouteEdge, reconnectEdge, deleteEdge) where

import Control.Arrow ((&&&))
import qualified Data.Maybe as Mb
import qualified Data.Graph.Inductive as G
import qualified Data.VectorSpace as V
import Data.VectorSpace ((^+^), (^-^), (<.>))
import qualified GHC.Exts as Ext

import Debug.Trace

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

type Point = (Double, Double)
type Segment = (Point, Point)
type LineString = [Point]

liftG :: Direction -> (LineString -> LineString) -> LineString -> LineString
liftG Forward f = f
liftG Reverse f = reverse . f . reverse

segments :: LineString -> [Segment]
segments ls = ls `zip` tail ls

projection :: Point -> Segment -> Point
projection xy (a, b) = let l2 = V.magnitudeSq $ b ^-^ a
                           t = ((xy ^-^ a) <.> (b ^-^ a)) / l2
                       in if l2 == 0 || t < 0 then a
                          else if t > 1 then b
                               else V.lerp a b t

segmentDistance :: Point -> (Point, Point) -> Double
segmentDistance xy seg = V.magnitude (xy ^-^ projection xy seg)


makeGeometry :: (Point, Double) -> (Point, Double) -> LineString
makeGeometry ((x0, y0), r0) ((x1, y1), r1) = [(x0', y0'), (x1', y1')]
    where len = V.magnitude $ (x1, y1) ^-^ (x0, y0)
          (x0', y0') = V.lerp (x0, y0) (x1, y1) (r0/len)
          (x1', y1') = V.lerp (x1, y1) (x0, y0) (r1/len)

snapGeometry xy0 xy1 g
    | length g > 2 = snapHead xy0 . liftG Reverse (snapHead xy1) $ g
    | otherwise    = makeGeometry (xy0, 10) (xy1, 10)
    where snapHead xy0 (_:xys@(xy:_)) =
              V.lerp xy0 xy (nodeRadius / V.magnitude (xy ^-^ xy0)) : xys

moveGeometry dxy = map (^+^ dxy)

{- Auxiliary: Graph operations -}

mapNodes :: G.DynGraph gr => (G.LNode a -> a) -> gr a b -> gr a b
mapNodes f g = if G.isEmpty g then g
               else let ((inLinks, nodeId, nodeLabel, outLinks), g') = G.matchAny g
                    in (inLinks, nodeId, f (nodeId, nodeLabel), outLinks) G.& mapNodes f g'

node :: G.Graph gr => gr a b -> G.Node -> G.LNode a
node g n = (n, l) where Just l = G.lab g n

labNeighbors :: G.Graph gr => gr a b -> G.Node -> [G.LNode a]
labNeighbors g n = map (node g) $ G.neighbors g n

labNeighborsL :: G.Graph gr => gr a b -> G.LNode a -> [G.LNode a]
labNeighborsL g (n, _) = labNeighbors g n

{- The Model -}

data Model = Model Graph deriving (Show)

empty :: Model
empty = Model G.empty

star :: Model
star = foldr addEdge' (foldr addNode' empty points) lines
    where points :: [(G.Node, Point)]
          points = [(i, (x, y)) | i <- [1..5] :: [G.Node],
                    let x = 300 + 200 * sin (2*pi/5 * fromIntegral i)
                        y = 225 - 200 * cos (2*pi/5 * fromIntegral i)]
          lines = [(i, j, makeGeometry (xy i) (xy j)) | i <- [1..5], let j = (i+1) `mod` 5 + 1]
          xy i = (Mb.fromJust $ lookup i points, nodeRadius)
          addNode' (i, xy) = addNode xy
          addEdge' (u, v, geom) m@(Model g) = addEdge Forward (node g u) (node g v) geom m


type Graph = G.Gr ElementLabel ()

data ElementLabel = NodeLabel Point
                  | EdgeLabel LineString
                  | EdgeEndLabel Direction
                  deriving (Show)
 
type Element = G.LNode ElementLabel

isNode :: Element -> Bool
isNode (_, NodeLabel _) = True
isNode _ = False

isEdge :: Element -> Bool
isEdge (_, EdgeLabel _) = True
isEdge _ = False

isEdgeEnd :: Element -> Bool
isEdgeEnd (_, EdgeEndLabel _) = True
isEdgeEnd _ = False


modelElements :: Model -> [Element]
modelElements (Model g) = G.labNodes g

modelNodes :: Model -> [Element]
modelNodes = filter isNode . modelElements

modelEdges :: Model -> [Element]
modelEdges = filter isEdge . modelElements

modelEdgeEnds :: Model -> [Element]
modelEdgeEnds = filter isEdgeEnd . modelElements


nodeXY :: Model -> Element -> Point
nodeXY _ (_, NodeLabel xy) = xy

nodeEnds :: Model -> Element -> [Element]
nodeEnds (Model g) (n, NodeLabel _) = filter isEdgeEnd $ labNeighbors g n

nodeEdges :: Model -> Element -> [Element]
nodeEdges m@(Model g) n@(_, NodeLabel _) =
    filter isEdge $ concatMap (labNeighborsL g) $ nodeEnds m n


edgeGeometry :: Model -> Element -> LineString
edgeGeometry _ (_, EdgeLabel geom) = geom

edgeEnds :: Model -> Element -> [Element]
edgeEnds (Model g) (e, EdgeLabel _) = filter isEdgeEnd $ labNeighbors g e

edgeEnd :: Model -> Direction -> Element -> Element
edgeEnd m@(Model g) dir e = head $ filter ((== dir) . endDir) $ edgeEnds m e
    where endDir (_, EdgeEndLabel dir') = dir'

edgeNodes :: Model -> Element -> [Element]
edgeNodes m@(Model g) e@(_, EdgeLabel _) =
    filter isNode $ concatMap (labNeighborsL g) $ edgeEnds m e


endXY :: Model -> Element -> Point
endXY m@(Model g) end@(_, EdgeEndLabel dir) = geomEnd dir geom
    where (_, EdgeLabel geom) = endEdge m end

endNode :: Model -> Element -> Element
endNode (Model g) (e, EdgeEndLabel _) = head $ filter isNode $ labNeighbors g e

endEdge :: Model -> Element -> Element
endEdge (Model g) (e, EdgeEndLabel _) = head $ filter isEdge $ labNeighbors g e

endOthers :: Model -> Element -> [Element]
endOthers m@(Model g) end@(e, EdgeEndLabel _) =
    filter ((/= e) . fst) $ edgeEnds m $ endEdge m end

endOther :: Model -> Element -> Element
endOther m@(Model g) end@(_, EdgeEndLabel _) = head $ endOthers m end


mapElements :: (Element -> ElementLabel) -- labeled node -> new label
               -> ((Element, Element, Element) -> ElementLabel) -- end, node, edge -> new label
               -> ((Element, [Element], [Element]) -> ElementLabel) -- edge, ends, nodes -> new label
               -> Model -> Model
mapElements fNode fEnd fEdge m@(Model g) = Model $ mapNodes f g
    where f n@(_, NodeLabel _) = fNode n
          f end@(_, EdgeEndLabel _) = fEnd (end, node, edge)
              where node = endNode m end
                    edge = endEdge m end
          f e@(ee, EdgeLabel _) = fEdge (e, ends, nodes)
              where ends = edgeEnds m e
                    nodes = edgeNodes m e


type HitTest = Maybe Element

distanceTo :: Model -> Point -> Element -> (Int, Double)
distanceTo _ xy   (_, NodeLabel xy')  = (0, V.magnitude (xy ^-^ xy'))
distanceTo m xy e@(_, EdgeEndLabel _) = (1, V.magnitude (xy ^-^ endXY m e))
distanceTo _ xy   (_, EdgeLabel geom) = (2, minimum $ map (segmentDistance xy) $ segments geom)

hitTest :: Model -> Point -> HitTest
hitTest m@(Model g) xy = g # G.labNodes
                         # map (id &&& distanceTo m xy)
                         # filter ((< 10) . snd . snd)
                         # Ext.sortWith snd
                         # map fst
                         # Mb.listToMaybe
    where (#) = flip ($)


type ModelTransition = Model -> Model

liftModel :: (Graph -> Graph) -> ModelTransition
liftModel f (Model g) = Model $ f g


addNode :: Point -> ModelTransition
addNode xy (Model g) = Model $ G.insNode (n, NodeLabel xy) g
    where [n] = G.newNodes 1 g

moveNode :: Element -> Point -> ModelTransition
moveNode (n, NodeLabel _) xy' = mapElements fixNode fixEnd fixEdge
    where fixNode (u, uv@(NodeLabel _))
              | u == n     = NodeLabel xy'
              | otherwise = uv
          fixEdge (e@(_, EdgeLabel _),
                   ends@[(_, EdgeEndLabel Reverse), (_, EdgeEndLabel Forward)],
                   nodes@[(_, NodeLabel _), (_, NodeLabel _)]) =
              fixEdge (e, reverse ends, reverse nodes)
          fixEdge ((_, ev@(EdgeLabel geom)),
                   [(_, EdgeEndLabel Forward), (_, EdgeEndLabel Reverse)],
                   [(u, NodeLabel uxy), (v, NodeLabel vxy)])
              | u == n && v == n = EdgeLabel $ moveGeometry (xy' ^-^ uxy) $ geom
              | u == n     = EdgeLabel $ snapGeometry xy' vxy $ geom
              | v == n     = EdgeLabel $ snapGeometry uxy xy' $ geom
              | otherwise = ev
          fixEnd ((_, endv@(EdgeEndLabel _)), _, _) = endv

deleteNode :: Element -> ModelTransition
deleteNode n@(_, NodeLabel _) m@(Model g) = Model $ G.delNodes (map fst $ n : es ++ ends) g
    where es = nodeEdges m n
          ends = concatMap (edgeEnds m) es


addEdge :: Direction -> Element -> Element -> LineString -> ModelTransition
addEdge Reverse v u geom m = addEdge Forward u v (reverse geom) m
addEdge Forward (u, NodeLabel uxy) (v, NodeLabel vxy) geom (Model g) =
    Model
    . G.insEdges [(ue, u, ()), (ue, e, ()), (ve, v, ()), (ve, e, ())]
    . G.insNodes [(e, EdgeLabel $ snapGeometry uxy vxy geom),
                  (ue, EdgeEndLabel Forward), (ve, EdgeEndLabel Reverse)]
    $ g
    where [e, ue, ve] = G.newNodes 3 g

rerouteEdge :: Element -> LineString -> ModelTransition
rerouteEdge e@(eId, EdgeLabel _) geom = mapElements fNode fEnd fEdge
    where fNode (_, nl@(NodeLabel _)) = nl
          fEnd ((_, endl@(EdgeEndLabel _)), _, _) = endl
          fEdge (e', ends@[(_, EdgeEndLabel Reverse), _], nodes) =
              fEdge (e', reverse ends, reverse nodes)
          fEdge ((eId', el'@(EdgeLabel geom')),
                 [(_, EdgeEndLabel Forward), (_, EdgeEndLabel Reverse)],
                 [(_, NodeLabel uxy), (_, NodeLabel vxy)])
              | eId' == eId = EdgeLabel $ snapGeometry uxy vxy geom
              | otherwise = el'

reconnectEdge :: Element -> Direction -> Element -> ModelTransition
reconnectEdge e@(_, EdgeLabel geom) dir (vId, NodeLabel _) m
    | vId == fst (edgeEnd m (dirReverse dir) e) = m
reconnectEdge e@(eId, EdgeLabel geom) dir (v', NodeLabel v'xy) m@(Model g) =
    liftModel (G.insEdges [(v'e, v', ()), (v'e, eId, ())] -- connect new end
               . G.insNode (v'e, vel) -- create new edge end of the same direction as old end
               . G.delNode ve) -- disconnect from old node
    . mapElements fNode fEnd fEdge -- modify geometry
    $ m
    where fNode (_, nl@(NodeLabel _)) = nl
          fEnd ((_, endl@(EdgeEndLabel _)), _, _) = endl
          fEdge ((eId', el'@(EdgeLabel geom')),
                 [(_, EdgeEndLabel dir1), (_, EdgeEndLabel dir2)],
                 [(_, NodeLabel xy1), (_, NodeLabel xy2)])
              | eId' == eId = EdgeLabel $ liftG dir (snapGeometry uxy v'xy) geom'
              | otherwise = el'
              where uxy = geomEnd (dirReverse dir) [xy1, xy2]
          (ve, vel@(EdgeEndLabel _)) = edgeEnd m (dirReverse dir) e
          [v'e] = G.newNodes 1 g

deleteEdge :: Element -> ModelTransition
deleteEdge e@(_, EdgeLabel _) m@(Model g) = Model $ G.delNodes (map fst $ e : ends) g
    where ends = edgeEnds m e


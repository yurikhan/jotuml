{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Model
       (Point, Direction (..), dirReverse, LineString, makeGeometry, liftG,
        Model, empty, star, modelNodes, modelEdges, modelEdgeEnds,
        Element, ElementLabel (..),
        Node (), nodeXY, nodeEnds, nodeEdges, nodeRadius, nodeMove,
        Edge (), edgeGeometry, edgeEnds, edgeNodes,
        EdgeEnd (), endDirection, endXY, endNode, endEdge, endOthers, endOther,
        HitTest (..), hitTest,
        ModelAction, idModel,
        addNode, moveNode, deleteNode,
        addEdge, rerouteEdge, reconnectEdge, deleteEdge) where

import Control.Arrow ((&&&))
import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad.State.Lazy as S
import qualified Data.Graph.Inductive as G
import qualified Data.List as L
import qualified Data.Maybe as Mb
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

data ElementLabel = NodeLabel Point
                  | EdgeLabel LineString
                  | EdgeEndLabel Direction
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

sameElement :: ElementClass elt => elt -> elt -> Bool
sameElement e e' = fst (toElement e) == fst (toElement e')

isNode :: Element -> Bool
isNode (_, NodeLabel _) = True
isNode _ = False

isEdge :: Element -> Bool
isEdge (_, EdgeLabel _) = True
isEdge _ = False

isEdgeEnd :: Element -> Bool
isEdgeEnd (_, EdgeEndLabel _) = True
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


edgeGeometry :: Model -> Edge -> LineString
edgeGeometry _ (Edge (_, EdgeLabel geom)) = geom

edgeEndNodeXY :: Model -> Direction -> Edge -> Point
edgeEndNodeXY m dir e = nodeXY m . endNode m $ edgeEnd m dir e

edgeEnds :: Model -> Edge -> [EdgeEnd]
edgeEnds m (Edge e) = map EdgeEnd . filter isEdgeEnd $ elementNeighbors m e

edgeEnd :: Model -> Direction -> Edge -> EdgeEnd
edgeEnd m dir e = head . filter ((== dir) . endDirection m) $ edgeEnds m e

edgeNodes :: Model -> Edge -> [Node]
edgeNodes m e = map (endNode m) $ edgeEnds m e


endDirection :: Model -> EdgeEnd -> Direction
endDirection m (EdgeEnd (_, EdgeEndLabel dir)) = dir

endXY :: Model -> EdgeEnd -> Point
endXY m end = geomEnd (endDirection m end) . edgeGeometry m $ endEdge m end

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
    where f n@(_, NodeLabel _) = fNode $ Node n
          f end@(_, EdgeEndLabel _) = fEnd (end', node, edge)
              where node = endNode m end'
                    edge = endEdge m end'
                    end' = EdgeEnd end
          f e@(ee, EdgeLabel _) = fEdge (edge, ends, nodes)
              where ends = edgeEnds m edge
                    nodes = edgeNodes m edge
                    edge = Edge e


data HitTest = OnNode Node
             | OnEdge Edge
             | OnEdgeEnd EdgeEnd
             | Nowhere
             deriving (Show)

distanceTo :: Model -> Point -> Element -> (Int, Double)
distanceTo _ xy   (_, NodeLabel xy')  = (0, V.magnitude (xy ^-^ xy'))
distanceTo m xy e@(_, EdgeEndLabel _) = (1, V.magnitude (xy ^-^ endXY m (EdgeEnd e)))
distanceTo _ xy   (_, EdgeLabel geom) = (2, minimum $ map (segmentDistance xy) $ segments geom)

hitTest :: Model -> Point -> HitTest
hitTest m xy = case map fst
                    . Ext.sortWith snd
                    . filter ((< 10) . snd . snd)
                    . map (id &&& distanceTo m xy)
                    $ modelElements m of
                   [] -> Nowhere
                   n@(_, NodeLabel _) : _ -> OnNode $ Node n
                   e@(_, EdgeLabel _) : _ -> OnEdge $ Edge e
                   end@(_, EdgeEndLabel _) : _ -> OnEdgeEnd $ EdgeEnd end


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
              | u == n     = EdgeLabel $ snapGeometry xy' vxy $ geom
              | v == n     = EdgeLabel $ snapGeometry uxy xy' $ geom
              | otherwise = ev
          fixEnd (EdgeEnd (_, endv), _, _) = endv

deleteNode :: Node -> ModelAction ()
deleteNode n = do
    m <- get
    let es = nodeEdges m n
        ends = concatMap (edgeEnds m) es
    modifyGraph $ G.delNodes (map fst $ toElement n : map toElement es ++ map toElement ends)


addEdge :: Direction -> Node -> Node -> LineString -> ModelAction (Edge, [EdgeEnd])
addEdge Reverse v u geom = addEdge Forward u v (reverse geom)
addEdge Forward (Node (u, NodeLabel uxy)) (Node (v, NodeLabel vxy)) geom = do
    [e, ue, ve] <- G.newNodes 3 <$> getGraph
    let ee = (e, EdgeLabel $ snapGeometry uxy vxy geom)
        uee = (ue, EdgeEndLabel Forward)
        vee = (ve, EdgeEndLabel Reverse)
    modifyGraph
        $ G.insEdges [(ue, u, ()), (ue, e, ()), (ve, v, ()), (ve, e, ())]
        . G.insNodes [ee, uee, vee]
    return (Edge ee, map EdgeEnd [uee, vee])

rerouteEdge :: Edge -> LineString -> ModelAction ()
rerouteEdge e@(Edge (eId, _)) geom = do
    m <- get
    let [uxy, vxy] = map (\dir -> edgeEndNodeXY m dir e) [Forward, Reverse]
    modifyGraph $ modifyNodeLabel eId $ \(_, EdgeLabel _) ->
        EdgeLabel $ snapGeometry uxy vxy geom

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
                                EdgeLabel $ liftG dir (snapGeometry uxy v'xy) geom')

deleteEdge :: Edge -> ModelAction ()
deleteEdge e = do
    ends <- edgeEnds <$> get <*> pure e
    modifyGraph $ G.delNodes (map fst $ toElement e : map toElement ends)

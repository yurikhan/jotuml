module Model
       (Point, Direction (..), dirReverse, LineString, makeGeometry, liftG,
        Model, empty, star, modelNodes, modelEdges, modelEdgeEnds,
        Element, ElementLabel (..),
        nodeXY, nodeEnds, nodeEdges, nodeRadius,
        edgeGeometry, edgeEnds, edgeNodes,
        endXY, endNode, endEdge, endOthers, endOther,
        HitTest, hitTest,
        ModelAction, idModel,
        addNode, moveNode, deleteNode,
        addEdge, rerouteEdge, reconnectEdge, deleteEdge) where

import Control.Arrow ((&&&))
import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad.State.Lazy as S
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
          addNodes :: ModelAction [Element]
          addNodes = mapM addNode points
          addEdges :: [Element] -> ModelAction [(Element, [Element])]
          addEdges ns = zipWithM addEdge' ns (drop 2 $ cycle ns)
          addEdge' :: Element -> Element -> ModelAction (Element, [Element])
          addEdge' u v = addEdge Forward u v =<< geom u v
          geom :: Element -> Element -> ModelAction LineString
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
nodeXY _ elt = traceShow elt undefined

nodeEnds :: Model -> Element -> [Element]
nodeEnds (Model g) (n, NodeLabel _) = filter isEdgeEnd $ labNeighbors g n

nodeEdges :: Model -> Element -> [Element]
nodeEdges m@(Model g) n@(_, NodeLabel _) =
    filter isEdge $ concatMap (labNeighborsL g) $ nodeEnds m n


edgeGeometry :: Model -> Element -> LineString
edgeGeometry _ (_, EdgeLabel geom) = geom

edgeEndNodeXY :: Model -> Direction -> Element -> Point
edgeEndNodeXY m dir e = nodeXY m $ endNode m $ edgeEnd m dir e

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


addNode :: Point -> ModelAction Element
addNode xy = do
    [nId] <- G.newNodes 1 <$> getGraph
    let n = (nId, NodeLabel xy)
    modifyGraph $ G.insNode n
    return n

moveNode :: Element -> Point -> ModelAction ()
moveNode (n, NodeLabel _) xy' = modify $ mapElements fixNode fixEnd fixEdge
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

deleteNode :: Element -> ModelAction ()
deleteNode n@(_, NodeLabel _) = do
    m <- get
    let es = nodeEdges m n
        ends = concatMap (edgeEnds m) es
    modifyGraph $ G.delNodes (map fst $ n : es ++ ends)


addEdge :: Direction -> Element -> Element -> LineString -> ModelAction (Element, [Element])
addEdge Reverse v u geom = addEdge Forward u v (reverse geom)
addEdge Forward (u, NodeLabel uxy) (v, NodeLabel vxy) geom = do
    [e, ue, ve] <- G.newNodes 3 <$> getGraph
    let ee = (e, EdgeLabel $ snapGeometry uxy vxy geom)
        uee = (ue, EdgeEndLabel Forward)
        vee = (ve, EdgeEndLabel Reverse)
    modifyGraph
        $ G.insEdges [(ue, u, ()), (ue, e, ()), (ve, v, ()), (ve, e, ())]
        . G.insNodes [ee, uee, vee]
    return (ee, [uee, vee])

rerouteEdge :: Element -> LineString -> ModelAction ()
rerouteEdge e@(eId, EdgeLabel _) geom = do
    m <- get
    let [uxy, vxy] = map (\dir -> edgeEndNodeXY m dir e) [Forward, Reverse]
    modifyGraph $ modifyNodeLabel eId $ \(_, EdgeLabel _) ->
        EdgeLabel $ snapGeometry uxy vxy geom

reconnectEdge :: Element -> Direction -> Element -> ModelAction ()
reconnectEdge e@(eId, EdgeLabel geom) dir (v'Id, NodeLabel v'xy) = do
    m <- get
    let uxy = edgeEndNodeXY m dir e
        ve@(veId, vel@(EdgeEndLabel _)) = edgeEnd m (dirReverse dir) e
        v@(vId, NodeLabel vxy) = endNode m ve
        [v'eId] = G.newNodes 1 $ modelGraph m
    when (v'Id /= vId) $
        modifyGraph
        $ G.insEdges [(v'eId, v'Id, ()), (v'eId, eId, ())]
        . G.insNode (v'eId, vel)
        . G.delNode veId
        . modifyNodeLabel eId (\(_, EdgeLabel geom') ->
                                EdgeLabel $ liftG dir (snapGeometry uxy v'xy) geom')

deleteEdge :: Element -> ModelAction ()
deleteEdge e@(_, EdgeLabel _) = do
    ends <- edgeEnds <$> get <*> pure e
    modifyGraph $ G.delNodes (map fst $ e : ends)

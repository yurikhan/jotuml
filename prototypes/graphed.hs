{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
import Control.Exception.Base
import Control.Monad
import Data.Graph.Inductive hiding (Node, Edge, Graph, Context, context)
import qualified Data.Graph.Inductive as G
import Data.IORef
import Data.Maybe
import Data.VectorSpace
import Debug.Trace
import GHC.Exts
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk hiding (Menu, Point)
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Windows.MessageDialog


trace' :: Show a => a -> a
trace' x = traceShow x x


{- Auxiliary: Geometry -}

type Point = (Double, Double)
type Segment = (Point, Point)
type LineString = [Point]

segments :: LineString -> [Segment]
segments ls = ls `zip` tail ls

projection :: Point -> Segment -> Point
projection xy (a, b) = let l2 = magnitudeSq $ b ^-^ a
                           t = ((xy ^-^ a) <.> (b ^-^ a)) / l2
                       in if l2 == 0 || t < 0 then a
                          else if t > 1 then b
                               else lerp a b t

segmentDistance :: Point -> (Point, Point) -> Double
segmentDistance xy seg = magnitude (xy ^-^ projection xy seg)

{- Auxiliary: Graphs -}

type NodeTransform a = LNode a -> a

mapNodes :: DynGraph gr => NodeTransform a -> gr a b -> gr a b
mapNodes f g = if isEmpty g then g
               else let ((inLinks, nodeId, nodeLabel, outLinks), g') = matchAny g
                    in (inLinks, nodeId, f (nodeId, nodeLabel), outLinks) & mapNodes f g'


type EdgeTransform a b = (G.Node, a, G.Node, a, b) -> b

mapEdges :: DynGraph gr => EdgeTransform a b -> gr a b -> gr a b
mapEdges f g = mkGraph ns' es'
               where ns' = labNodes g
                     es' = map f' es
                     es = labEdges g
                     f' e@(u, v, l) = (u, v, f $ nodeLabels e)
                     nodeLabels (u, v, l) = (u, fromJust $ lab g u, v, fromJust $ lab g v, l)

node :: G.Graph gr => gr a b -> G.Node -> LNode a
node g i = (i, v) where Just v = lab g i

labNeighbors :: G.Graph gr => gr a b -> G.Node -> [LNode a]
labNeighbors g n = map (node g) $ neighbors g n

labNeighborsL :: G.Graph gr => gr a b -> LNode a -> [LNode a]
labNeighborsL g (n, _) = labNeighbors g n

{- The Model -}

nodeRadius :: Double
nodeRadius = 10

type Graph = Gr ElementView ()
data Model = Model Graph deriving (Show)
type ModelRef = IORef Model

data ElementView =
    NodeView Point
    | EdgeView LineString
    | EdgeEndView Direction
    deriving (Show)
type Element = LNode ElementView

isNode :: Element -> Bool
isNode (_, NodeView {}) = True
isNode _ = False

isEdge :: Element -> Bool
isEdge (_, EdgeView {}) = True
isEdge _ = False

isEdgeEnd :: Element -> Bool
isEdgeEnd (_, EdgeEndView _) = True
isEdgeEnd _ = False

nodeEnds :: Graph -> Element -> [Element]
nodeEnds g (n, NodeView {}) = filter isEdgeEnd $ labNeighbors g n

nodeEdges :: Graph -> Element -> [Element]
nodeEdges g n@(_, NodeView {}) = filter isEdge $ concatMap (labNeighborsL g) $ nodeEnds g n

edgeEnds :: Graph -> Element -> [Element]
edgeEnds g (e, EdgeView {}) = filter isEdgeEnd $ labNeighbors g e

edgeNodes :: Graph -> Element -> [Element]
edgeNodes g e@(_, EdgeView {}) = filter isNode $ concatMap (labNeighborsL g) $ edgeEnds g e

endNode :: Graph -> Element -> Element
endNode g (e, EdgeEndView _) = head $ filter isNode $ labNeighbors g e

endEdge :: Graph -> Element -> Element
endEdge g (e, EdgeEndView _) = head $ filter isEdge $ labNeighbors g e

endOthers :: Graph -> Element -> [Element]
endOthers g end@(e, EdgeEndView _) = filter ((/= e) . fst) $ edgeEnds g $ endEdge g end

endOther :: Graph -> Element -> Element
endOther g end@(_, EdgeEndView _) = head $ endOthers g end

endXY :: Graph -> Element -> Point
endXY g end@(_, EdgeEndView dir) = geomEnd dir geom
    where (_, EdgeView geom) = endEdge g end
          geomEnd Forward = head
          geomEnd Reverse = last

mapElements :: (Element -> ElementView) -- labeled node -> new label
               -> ((Element, Element, Element) -> ElementView) -- end, node, edge -> new label
               -> ((Element, [Element], [Element]) -> ElementView) -- edge, [end], [node] -> new label
               -> Graph -> Graph
mapElements fNode fEnd fEdge g = mapNodes f g
    where f n@(_, NodeView _) = fNode n
          f end@(_, EdgeEndView _) = fEnd (end, node, edge)
              where node = endNode g end
                    edge = endEdge g end
          f e@(ee, EdgeView _) = fEdge (e, ends, nodes)
              where ends = edgeEnds g e
                    nodes = edgeNodes g e

nodeNear :: Point -> Element -> Bool
nodeNear xy (_, NodeView xy') = magnitude (xy ^-^ xy') < nodeRadius

type Reference = LEdge ()

endNear :: Graph -> Point -> Element -> Bool
endNear g xy end@(_, EdgeEndView dir) = magnitude (xy ^-^ (endXY g end)) < 10

edgeNear :: Point -> Element -> Bool
edgeNear xy (_, EdgeView geom) = any (segmentNear xy) (segments geom)

segmentNear :: Point -> (Point, Point) -> Bool
segmentNear xy seg = (segmentDistance xy seg) < 10

type HitTest = Maybe Element

distanceTo :: Graph -> Point -> Element -> (Int, Double)
distanceTo _ xy (_, NodeView xy') = (0, magnitude (xy ^-^ xy'))
distanceTo g xy end@(_, EdgeEndView _) = (1, magnitude (xy ^-^ endXY g end))
distanceTo _ xy (_, EdgeView g) = (2, minimum $ map (segmentDistance xy) $ segments g)

hitTest :: Point -> Graph -> HitTest
hitTest xy g = listToMaybe $ sortWith (distanceTo g xy) nearThings
    where nearNodes = filter (nodeNear xy) $ filter isNode $ labNodes g
          nearEnds = filter (endNear g xy) $ filter isEdgeEnd $ labNodes g
          nearEdges = filter (edgeNear xy) $ filter isEdge $ labNodes g
          nearThings = nearNodes ++ nearEnds ++ nearEdges

makeGeometry :: (Point, Double) -> (Point, Double) -> LineString
makeGeometry ((x0, y0), r0) ((x1, y1), r1) = [(x0', y0'), (x1', y1')]
    where len = magnitude $ (x1, y1) ^-^ (x0, y0)
          (x0', y0') = lerp (x0, y0) (x1, y1) (r0/len)
          (x1', y1') = lerp (x1, y1) (x0, y0) (r1/len)

snapGeometry xy0 xy1 geom@(_:_:_:_) = snapHead xy0 . liftG Reverse (snapHead xy1) $ geom
    where snapHead xy0 (_:xys@(xy:_)) = lerp xy0 xy (nodeRadius / magnitude (xy ^-^ xy0)) : xys
snapGeometry xy0 xy1 (_:_:_) = makeGeometry (xy0, 10) (xy1, 10)

moveGeometry dxy = map (^+^ dxy)


initialModel :: Model
initialModel = let points = [(i, (x, y)) | i <- [0..4],
                             let x = 300 + 200 * sin(2*pi/5 * fromIntegral i)
                                 y = 225 - 200 * cos(2*pi/5 * fromIntegral i)]
                   lines = [(i, makeGeometry (xyFromI i) (xyFromI j)) |
                            i <- [0..4], let j = (i+2) `mod` 5]
                   ns = [(i, NodeView xy) | (i, xy) <- points]
                   es = [(i + 5, EdgeView geom) | (i, geom) <- lines]
                   ends = [[(i * 2 + 10, EdgeEndView Forward),
                            (i * 2 + 11, EdgeEndView Reverse)] | i <- [0..4]]
                   refs = [[(i * 2 + 10, i, ()), (i * 2 + 11, i, ()),
                            (i * 2 + 10, i + 5, ()), (i * 2 + 11, 5 + (i + 3) `mod` 5, ())]
                          | i <- [0..4]]
                   xyFromI i = (fromJust $ lookup i points, nodeRadius)
               in Model $ mkGraph (ns ++ es ++ concat ends) (concat refs)
               -- in Model $ mkGraph [] []

type ModelTransition = Model -> Model

liftModel :: (Graph -> Graph) -> ModelTransition
liftModel f (Model g) = Model (f g)

moveNode :: Element -> Point -> ModelTransition
moveNode (n, _) xy' = liftModel $ mapElements fixNode fixEnd fixEdge
    where fixNode (u, NodeView _) | u == n = NodeView xy'
          fixNode (_, uv@(NodeView _)) = uv
          fixEnd ((_, endv@(EdgeEndView _)), _, _) = endv
          fixEdge ((_, EdgeView geom), _, [(u, NodeView uxy), (v, NodeView _)])
              | u == n && v == n = EdgeView $ moveGeometry (xy' ^-^ uxy) $ geom
          fixEdge (e@(_, EdgeView _),
                   ends@[(_, EdgeEndView Reverse), (_, EdgeEndView Forward)],
                   nodes@[(v, NodeView vxy), (u, NodeView uxy)]) =
              fixEdge (e, reverse ends, reverse nodes)
          fixEdge ((_, EdgeView geom),
                   [(_, EdgeEndView Forward), (_, EdgeEndView Reverse)],
                   [(u, NodeView uxy), (v, NodeView vxy)])
              | u == n = EdgeView $ snapGeometry xy' vxy $ geom
              | v == n = EdgeView $ snapGeometry uxy xy' $ geom
          fixEdge ((_, ev), _, _) = ev

deleteNode :: Element -> ModelTransition
deleteNode n@(_, NodeView _) = liftModel $ \g ->
    delNodes (map fst $ n : concatMap (\e -> e : edgeEnds g e) (nodeEdges g n))
    $ g

addEdge :: Direction -> Element -> Element -> LineString -> ModelTransition
addEdge Reverse v u geom = addEdge Forward u v $ reverse geom
addEdge Forward (u, NodeView uxy) (v, NodeView vxy) geom = liftModel $ \g ->
    let [e, ue, ve] = newNodes 3 g in
    insEdges [(ue, u, ()), (ue, e, ()), (ve, v, ()), (ve, e, ())]
    . insNodes [(e, EdgeView $ snapGeometry uxy vxy geom),
                (ue, EdgeEndView Forward), (ve, EdgeEndView Reverse)]
    $ g

deleteEdge :: Element -> ModelTransition
deleteEdge e@(_, EdgeView _) = liftModel $ \g ->
    delNodes (map fst $ e : edgeEnds g e)
    $ g

{- The View -}

data View = View {
    builder :: Gtk.Builder,
    window :: Gtk.Window,
    menu :: Menu,
    nodePopupMenu :: Gtk.Menu,
    edgePopupMenu :: Gtk.Menu,
    layout :: Gtk.Layout,
    statusBar :: Gtk.Statusbar,
    layoutButtonPressHandler :: IORef (Click -> MouseButton -> Maybe (MouseButton, TimeStamp)
           -> (Double, Double) -> HitTest
           -> ModelAndViewUpdate (IO ())),
    layoutMotionNotifyHandler :: IORef (Point -> ViewAction),
    layoutButtonReleaseHandler :: IORef (Point -> ModelAndViewUpdate (IO ())),
    context :: IORef Context,
    elasticContext :: IORef (Maybe ElasticContext)
    }

data Menu = Menu {
    fileNewItem :: Gtk.MenuItem,
    fileOpenItem :: Gtk.MenuItem,
    fileSaveItem :: Gtk.MenuItem,
    fileSaveAsItem :: Gtk.MenuItem,
    fileQuitItem :: Gtk.MenuItem,
    editCutItem :: Gtk.MenuItem,
    editCopyItem :: Gtk.MenuItem,
    editPasteItem :: Gtk.MenuItem,
    editDeleteItem :: Gtk.MenuItem,
    helpAboutItem :: Gtk.MenuItem,
    nodeDeleteItem :: Gtk.MenuItem,
    edgeDeleteItem :: Gtk.MenuItem
    }

data Context = NoContext
             | NodeContext Element
             | EdgeContext Element
             deriving (Show)

data ElasticContext = ElasticContext Model ElasticElement

data ElasticElement = ElasticNode Element
                    | ElasticEdge Direction
                          Element -- fixed node
                          (Maybe Element) -- Nothing if new, Just edge otherwise
                          (Maybe Element) -- Nothing if new, Just other end otherwise
                          LineString -- current geometry

data Direction = Forward
               | Reverse
               deriving (Show)
liftG :: Direction -> (LineString -> LineString) -> LineString -> LineString
liftG Forward f = f
liftG Reverse f = reverse . f . reverse

dirReverse :: Direction -> Direction
dirReverse Forward = Reverse
dirReverse Reverse = Forward

type ViewAction = Model -> View -> IO ()

idView :: ViewAction
idView _ _ = return ()

clearElasticContext :: ViewAction
clearElasticContext model view = do
    writeIORef' (elasticContext view) Nothing

startElasticNode :: Element -> Point -> ViewAction
startElasticNode u@(uId, NodeView _) xy' model view = do
    writeIORef' (elasticContext view) $ Just $ ElasticContext m'
        $ ElasticNode (uId, NodeView xy')
    where m' = moveNode u xy' model

moveElasticNode :: Point -> ViewAction
moveElasticNode xy' _ view = do
    ec <- readIORef $ elasticContext view
    case ec of
        Just (ElasticContext model' (ElasticNode u)) -> startElasticNode u xy' model' view
        _ -> return ()

endElasticNode :: Point -> ModelAndViewUpdate (IO ())
endElasticNode xy' modelRef view = do
    ec <- readIORef $ elasticContext view
    (endElasticNode' ec <@> clearElasticContext >&> clearDragHandlers) modelRef view
    where
        endElasticNode' Nothing = id
        endElasticNode' (Just (ElasticContext model' (ElasticNode u))) = moveNode u xy'

startElasticEdge :: Direction -> Element -> Maybe Element -> Maybe Element -> Point -> ViewAction
startElasticEdge dir u@(_, NodeView xy) e v xy' model view = do
    writeIORef' (elasticContext view) $
        Just $ ElasticContext (maybe id deleteEdge e $ model) $ ElasticEdge dir u e v $ geom' e
    where geom' Nothing = makeGeometry (xy, 10) (xy', 0)
          geom' (Just (_, EdgeView geom)) = liftG dir ((++ [xy']) . init) geom

modifyElasticEdgeGeometry :: (LineString -> LineString) -> ViewAction
modifyElasticEdgeGeometry f _ view = do
    ec <- readIORef $ elasticContext view
    modify' ec
    where modify' Nothing = return ()
          modify' (Just (ElasticContext model' (ElasticEdge dir u e v geom))) =
              writeIORef' (elasticContext view) $
                  Just $ ElasticContext model' $ ElasticEdge dir u e v $ liftG dir f $ geom

moveElasticEdge :: Point -> ViewAction
moveElasticEdge xy' = modifyElasticEdgeGeometry ((++ [xy']) . init)

releaseElasticEdge :: Point -> ModelAndViewUpdate (IO ())
releaseElasticEdge xy' modelRef view = do
    ec <- readIORef $ elasticContext view
    Model graph <- readIORef modelRef
    (release' ec (hitTest xy' graph)) modelRef view
    where
        release' :: Maybe ElasticContext -> HitTest -> ModelAndViewUpdate (IO ())
        release' (Just (ElasticContext _ (ElasticEdge dir u Nothing Nothing g)))
                 (Just v'@(_, NodeView _)) =
            addEdge dir u v' g <@> clearElasticContext >&> clearDragHandlers
        release' (Just (ElasticContext _ (ElasticEdge dir u (Just e) (Just v) g)))
                 (Just v'@(_, NodeView _)) =
            addEdge dir u v' g . deleteEdge e
            <@> clearElasticContext >&> clearDragHandlers
        release' (Just (ElasticContext _ (ElasticEdge _ _ _ _ _))) _ =
            id <@> modifyElasticEdgeGeometry (++ [xy'])
        release' _ _ = id <@> clearElasticContext >&> clearDragHandlers

refreshView :: ViewAction
refreshView _ v = do
    widgetQueueDraw $ layout v

(>&>) :: ViewAction -> ViewAction -> ViewAction
infixl 7 >&>
a1 >&> a2 = \ m v -> do
    a1 m v
    a2 m v

createView :: IO View
createView = do
    builder <- builderNew
    builderAddFromFile builder "graphed.ui"

    window <- builderGetObject builder castToWindow "main_window"

    nodePopupMenu <- builderGetObject builder castToMenu "node_popup_menu"
    edgePopupMenu <- builderGetObject builder castToMenu "edge_popup_menu"

    fileNewItem <- builderGetObject builder castToMenuItem "file_new_item"
    fileOpenItem <- builderGetObject builder castToMenuItem "file_open_item"
    fileSaveItem <- builderGetObject builder castToMenuItem "file_save_item"
    fileSaveAsItem <- builderGetObject builder castToMenuItem "file_save_as_item"
    fileQuitItem <- builderGetObject builder castToMenuItem "file_quit_item"

    editCutItem <- builderGetObject builder castToMenuItem "edit_cut_item"
    editCopyItem <- builderGetObject builder castToMenuItem "edit_copy_item"
    editPasteItem <- builderGetObject builder castToMenuItem "edit_paste_item"
    editDeleteItem <- builderGetObject builder castToMenuItem "edit_delete_item"

    helpAboutItem <- builderGetObject builder castToMenuItem "help_about_item"

    nodeDeleteItem <- builderGetObject builder castToMenuItem "node_delete_item"
    edgeDeleteItem <- builderGetObject builder castToMenuItem "edge_delete_item"

    layout <- builderGetObject builder castToLayout "layout"

    statusBar <- builderGetObject builder castToStatusbar "status_bar"

    layoutButtonPressHandler <- newIORef pressed
    layoutMotionNotifyHandler <- newIORef $ const idView
    layoutButtonReleaseHandler <- newIORef $ const $ id <@> idView

    context <- newIORef NoContext

    elasticContext <- newIORef Nothing

    let view = View builder window
               (Menu fileNewItem fileOpenItem fileSaveItem fileSaveAsItem fileQuitItem
                editCutItem editCopyItem editPasteItem editDeleteItem helpAboutItem
                nodeDeleteItem edgeDeleteItem)
               nodePopupMenu edgePopupMenu layout statusBar
               layoutButtonPressHandler layoutMotionNotifyHandler layoutButtonReleaseHandler
               context elasticContext

    return view

renderView :: DrawWindow -> ViewAction
renderView drawWindow model view = renderWithDrawable drawWindow $ do
    setSourceRGB 0 0 0
    ec <- liftIO $ readIORef (elasticContext view)
    let Model graph' = maybe model elasticModel ec
    renderNodes $ filter isNode $ labNodes graph'
    renderEdges $ filter isEdge $ labNodes graph'
    renderElasticEdge ec

    where elasticModel :: ElasticContext -> Model
          elasticModel (ElasticContext m _) = m

          renderNodes :: [Element] -> Render ()
          renderNodes = mapM_ renderNode

          renderNode :: Element -> Render ()
          renderNode (_, NodeView (x,y)) = do
              arc x y nodeRadius 0 $ 2*pi
              stroke

          renderEdges :: [Element] -> Render ()
          renderEdges = mapM_ renderEdge

          renderEdge :: Element -> Render ()
          renderEdge (_, EdgeView g) = renderLineString g

          renderElasticEdge :: Maybe ElasticContext -> Render ()
          renderElasticEdge (Just (ElasticContext _ (ElasticEdge _ _ _ _ g))) =
              renderLineString g
          renderElasticEdge _ = return ()

          renderLineString :: LineString -> Render ()
          renderLineString [] = return ()
          renderLineString g@((x0,y0):xys) = do
              moveTo x0 y0
              mapM_ (uncurry lineTo) xys
              renderArrowhead 10 (pi/6) xyN1 xyN
              stroke
              where xyN = head yxs
                    xyN1 = head $ tail yxs
                    yxs = reverse g

          renderArrowhead :: Double -> Double -> Point -> Point -> Render ()
          renderArrowhead arrowSize psi (x,y) (x',y') = do
              let phi = atan2 (y' - y) (x' - x)
              moveTo (x' - arrowSize * cos (phi - psi)) (y' - arrowSize * sin (phi - psi))
              lineTo x' y'
              lineTo (x' - arrowSize * cos (phi + psi)) (y' - arrowSize * sin (phi + psi))
              stroke

showInStatus :: Show a => (Model -> a) -> ViewAction
showInStatus f m v = do
    let s = statusBar v
    contextId <- statusbarGetContextId s ""
    statusbarPush s contextId $ show $ f m
    return ()

popupNodeMenu :: Element -> Maybe (MouseButton, TimeStamp) -> ViewAction
popupNodeMenu u bt m v = do
    writeIORef' (context v) $ NodeContext u
    menuPopup (nodePopupMenu v) bt

popupEdgeMenu :: Element -> Maybe (MouseButton, TimeStamp) -> ViewAction
popupEdgeMenu e bt m v = do
    writeIORef' (context v) $ EdgeContext e
    menuPopup (edgePopupMenu v) bt

setDragHandlers :: (Point -> ViewAction)
                   -> (Point -> ModelAndViewUpdate (IO ()))
                   -> ViewAction
setDragHandlers dragging released _ view = do
    writeIORef' (layoutButtonPressHandler view) ignorePressed
    writeIORef' (layoutMotionNotifyHandler view) dragging
    writeIORef' (layoutButtonReleaseHandler view) released

clearDragHandlers :: ViewAction
clearDragHandlers _ view = do
    writeIORef' (layoutButtonPressHandler view) pressed
    writeIORef' (layoutMotionNotifyHandler view) $ const idView
    writeIORef' (layoutButtonReleaseHandler view) $ const $ id <@> idView

{- The Controller -}

controller :: Model -> View -> IO ()
controller m v = do
    modelRef <- newIORef m
    setupView modelRef v
    refreshView m v

type ModelAndViewUpdate r = ModelRef -> View -> r

(<@>) :: ModelTransition -> ViewAction -> ModelAndViewUpdate (IO ())
infix 6 <@>
modelTrans <@> viewAction = \ modelRef view -> do
    model <- readIORef modelRef
    let model' = modelTrans model
    writeIORef' modelRef $ model'
    viewAction model' view

(>>>) :: Monad m => ModelAndViewUpdate (m ())
         -> ModelAndViewUpdate (m ())
         -> ModelAndViewUpdate (m ())
infixl 5 >>>
upd1 >>> upd2 = \ modelRef v -> do
    upd1 modelRef v
    upd2 modelRef v

writeIORef' :: IORef a -> a -> IO ()
writeIORef' ref x = x `seq` writeIORef ref x


setupView :: ModelAndViewUpdate (IO ())
setupView modelRef view = do
    on (window view) objectDestroy mainQuit
    on (window view) deleteEvent $ liftIO $ do
        confirm (Just $ window view) quitConfirmation
            (return False) (return True)

    widgetAddEvents (layout view) [PointerMotionMask]
    on (layout view) buttonPressEvent $ run layoutButtonPressed
    on (layout view) motionNotifyEvent $ run layoutMotionNotify
    on (layout view) buttonReleaseEvent $ run layoutButtonReleased
    on (layout view) exposeEvent $ run layoutExposed

    on (nodeDeleteItem $ menu view) menuItemActivate $ run deleteActivated
    on (edgeDeleteItem $ menu view) menuItemActivate $ run deleteActivated

    on (helpAboutItem $ menu view) menuItemActivate $ run dumpGraph

    return ()

    where run handler = handler modelRef view

confirm :: Maybe Window -> String -> IO a -> IO a -> IO a
confirm parent text action inaction = do
    response <- bracket
                (messageDialogNew parent [DialogModal] MessageWarning ButtonsOkCancel text)
                widgetDestroy
                dialogRun
    if (response == ResponseOk) then action else inaction

quitConfirmation = "Do you really want to quit?"


layoutExposed :: ModelAndViewUpdate (EventM EExpose Bool)
layoutExposed m v = do
    drawWindow <- eventWindow
    liftIO $ (id <@> renderView drawWindow) m v
    return True

layoutButtonPressed :: ModelAndViewUpdate (EventM EButton Bool)
layoutButtonPressed m v = do
    button <- eventButton
    timestamp <- eventTime
    click <- eventClick
    xy <- eventCoordinates
    liftIO $ do
        Model graph <- readIORef m
        handler <- readIORef $ layoutButtonPressHandler v
        handler click button (Just (button, timestamp)) xy (hitTest xy graph) m v
    return True

pressed :: Click -> MouseButton -> Maybe (MouseButton, TimeStamp)
           -> (Double, Double) -> HitTest
           -> ModelAndViewUpdate (IO ())
pressed SingleClick LeftButton _ xy Nothing = addNewNode xy
pressed SingleClick LeftButton _ xy (Just n@(_, NodeView _)) =
    dragEdge Forward n Nothing Nothing xy
pressed SingleClick MiddleButton _ xy (Just n@(_, NodeView _)) = dragNode n xy
pressed SingleClick MiddleButton _ xy (Just end@(_, EdgeEndView dir)) = \m view -> do
    Model g <- readIORef m
    dragEdge (dirReverse dir) (endNode g $ endOther g end)
        (Just $ endEdge g end) (Just $ endNode g end) xy m view
pressed SingleClick RightButton bt _ (Just n@(_, NodeView _)) = id <@> popupNodeMenu n bt
pressed SingleClick RightButton bt _ (Just e@(_, EdgeView _)) = id <@> popupEdgeMenu e bt
pressed SingleClick RightButton bt _ (Just end@(_, EdgeEndView _)) = \m view -> do
    Model g <- readIORef m
    (id <@> popupEdgeMenu (endEdge g end) bt) m view
pressed _ _ _ _ _ = id <@> idView
ignorePressed _ _ _ _ _ = id <@> idView

layoutMotionNotify :: ModelAndViewUpdate (EventM EMotion Bool)
layoutMotionNotify m v = do
    xy <- eventCoordinates
    liftIO $ do
        handler <- readIORef $ layoutMotionNotifyHandler v
        (id <@> handler xy >&> refreshView >&> showInStatus (hitTest xy . \(Model g) -> g)) m v
    return True

layoutButtonReleased :: ModelAndViewUpdate (EventM EButton Bool)
layoutButtonReleased m v = do
    xy <- eventCoordinates
    liftIO $ do
        handler <- readIORef $ layoutButtonReleaseHandler v
        (handler xy >>> id <@> refreshView) m v
    return True

addNewNode :: Point -> ModelAndViewUpdate (IO ())
addNewNode xy mref v = do
    Model graph <- readIORef mref
    let [n] = newNodes 1 graph
        node = (n, NodeView xy)
    (liftModel (insNode node) <@> refreshView) mref v
    dragNode node xy mref v

dragNode :: Element -> Point -> ModelAndViewUpdate (IO ())
dragNode n xy = id
                <@> startElasticNode n xy
                >&> refreshView
                >&> setDragHandlers moveElasticNode endElasticNode

dragEdge :: Direction -> Element -> Maybe Element -> Maybe Element -> Point
            -> ModelAndViewUpdate (IO ())
dragEdge dir fixedNode edge otherNode xy =
    id
    <@> startElasticEdge dir fixedNode edge otherNode xy
    >&> refreshView
    >&> setDragHandlers moveElasticEdge releaseElasticEdge

deleteActivated :: ModelAndViewUpdate (IO ())
deleteActivated m v = do
    ctx <- readIORef (context v)
    (delete' ctx <@> refreshView >>> dumpGraph) m v
    where
        delete' (NodeContext n) = deleteNode n
        delete' (EdgeContext e) = deleteEdge e
        delete' _ = id

dumpGraph :: ModelAndViewUpdate (IO ())
dumpGraph = id <@> dumpGraph'
    where dumpGraph' = flip $ const print

main :: IO ()
main = do
    initGUI
    view <- createView
    controller initialModel view
    mainGUI

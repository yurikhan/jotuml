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

{- The Model -}

nodeRadius :: Double
nodeRadius = 10

type Graph = Gr NodeView EdgeView
data Model = Model {
    graph :: Graph
    } deriving (Show)
type ModelRef = IORef Model

data NodeView = NodeView {
    xy :: Point,
    nodeLabel :: String
    } deriving (Show)
type Node = LNode NodeView

nodeNear :: Point -> Node -> Bool
nodeNear xy (_, NodeView xy' _) = magnitude (xy ^-^ xy') < nodeRadius

data EdgeView = EdgeView {
    geometry :: LineString,
    sourceLabel :: String,
    edgeLabel :: String,
    targetLabel :: String
    } deriving (Show)
type Edge = LEdge EdgeView
type EdgeEnds = (Node, Edge, Node)
type EdgeEndType = LineString -> Point -- head or last

edgeEnds :: Graph -> Edge -> EdgeEnds
edgeEnds g e@(i, j, _) = (node g i, e, node g j)

edgesEnds :: Graph -> [EdgeEnds]
edgesEnds g = edgeEnds g `map` labEdges g

endNear :: EdgeEndType -> Point -> EdgeEnds -> Bool
endNear t xy (_, (_, _, EdgeView g _ _ _), _) = magnitude (xy ^-^ t g) < 10

edgeNear :: Point -> Edge -> Bool
edgeNear xy (_, _, EdgeView g _ _ _) = any (segmentNear xy) (segments g)

segmentNear :: Point -> (Point, Point) -> Bool
segmentNear xy seg = (segmentDistance xy seg) < 10

data HitTest = OnNode Node
             | OnEdge Edge
             | OnEdgeStart EdgeEnds
             | OnEdgeEnd EdgeEnds
             | Nowhere
             deriving (Show)

distanceTo :: Point -> HitTest -> (Int, Double)
distanceTo xy (OnNode (_, NodeView xy' _)) = (0, magnitude (xy ^-^ xy'))
distanceTo xy (OnEdgeStart (_, (_, _, EdgeView g _ _ _), _)) = (1, magnitude (xy ^-^ head g))
distanceTo xy (OnEdgeEnd (_, (_, _, EdgeView g _ _ _), _)) = (1, magnitude (xy ^-^ last g))
distanceTo xy (OnEdge (_, _, EdgeView g _ _ _)) =
    (2, minimum $ map (segmentDistance xy) $ segments g)
distanceTo _ Nowhere = (9, 0)

hitTest :: Point -> Graph -> HitTest
hitTest xy g = case sortWith (distanceTo xy) nearThings of
                   [] -> Nowhere
                   ht:_ -> ht
    where nearNodes = filter (nodeNear xy) $ labNodes g
          nearStarts = filter (endNear head xy) $ edgesEnds g
          nearEnds = filter (endNear last xy) $ edgesEnds g
          nearEdges = filter (edgeNear xy) $ labEdges g
          nearThings = (map OnNode nearNodes) ++
                       (map OnEdgeStart nearStarts) ++
                       (map OnEdgeEnd nearEnds) ++
                       (map OnEdge nearEdges)

makeGeometry :: (Point, Double) -> (Point, Double) -> LineString
makeGeometry ((x0, y0), r0) ((x1, y1), r1) = [(x0', y0'), (x1', y1')]
    where len = magnitude $ (x1, y1) ^-^ (x0, y0)
          (x0', y0') = lerp (x0, y0) (x1, y1) (r0/len)
          (x1', y1') = lerp (x1, y1) (x0, y0) (r1/len)

initialModel :: Model
initialModel = let ns = [(i, NodeView (x, y) "") | i <- [0..4],
                         let x = 300 + 200 * sin(2*pi/5 * fromIntegral i)
                             y = 225 - 200 * cos(2*pi/5 * fromIntegral i)]
                   es = [(i, j, EdgeView (makeGeometry (xyFromI i) (xyFromI j)) "" "" "") |
                         i <- [0..4], let j = (i+2) `mod` 5]
                   xyFromI i = (xy $ fromJust $ lookup i ns, nodeRadius)
               in Model {graph=mkGraph ns es}

type ModelTransition = Model -> Model

liftModel :: (Graph -> Graph) -> ModelTransition
liftModel f m = m {graph=f $ graph m}

moveNode :: Node -> Point -> ModelTransition
moveNode (n, _) xy' = liftModel
                      $ mapEdges fixEdge
                      . mapNodes fixNode
    where fixEdge (u, uv, v, vv, ev) =
              if u == n || v == n
              then ev{geometry=makeGeometry (xy uv, nodeRadius) (xy vv, nodeRadius)}
              else ev
          fixNode (m, mv) = if m == n then mv{xy=xy'} else mv

deleteNode :: Node -> ModelTransition
deleteNode (n, _) = liftModel $ delNode n

addEdge :: Direction -> Node -> Node -> ModelTransition
addEdge Reverse v u = addEdge Forward u v
addEdge Forward (u, uv) (v, vv) =
    liftModel $ insEdge (u, v, EdgeView (makeGeometry (xy uv, 10) (xy vv, 10)) "" "" "")

deleteEdge :: Edge -> ModelTransition
deleteEdge (u, v, _) = liftModel $ delEdge (u, v)

{- The View -}

data View = View {
    builder :: Gtk.Builder,
    window :: Gtk.Window,
    menu :: Menu,
    nodePopupMenu :: Gtk.Menu,
    edgePopupMenu :: Gtk.Menu,
    layout :: Gtk.Layout,
    statusBar :: Gtk.Statusbar,
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
             | NodeContext Node
             | EdgeContext Edge
             deriving (Show)

data ElasticContext = ElasticContext Model ElasticElement

data ElasticElement = ElasticNode Node
                    | ElasticEdge Direction
                          Node -- fixed node
                          (Maybe Node) -- Nothing if new, Just other end otherwise
                          LineString -- current geometry

data Direction = Forward
               | Reverse
               deriving (Show)

type ViewAction = Model -> View -> IO ()

idView :: ViewAction
idView _ _ = return ()

clearElasticContext :: ViewAction
clearElasticContext model view = do
    writeIORef' (elasticContext view) Nothing

startElasticNode :: Node -> Point -> ViewAction
startElasticNode u@(uId, uv@(NodeView xy _)) xy' model view = do
    writeIORef (elasticContext view) $ Just $ ElasticContext m' $ ElasticNode (uId, uv{xy=xy'})
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
    (endElasticNode' ec <@> clearElasticContext) modelRef view
    where
        endElasticNode' Nothing = id
        endElasticNode' (Just (ElasticContext model' (ElasticNode u))) = moveNode u xy'

startElasticEdge :: Direction -> Node -> Maybe Node -> Point -> ViewAction
startElasticEdge dir u@(_, NodeView xy _) v xy' model view = do
    writeIORef' (elasticContext view) $ Just $ ElasticContext m'
        $ ElasticEdge dir u v geom'
    where geom' = makeGeometry (xy, 10) (xy', 0)
          m' = liftModel (mapEdges (setGeometry geom' dir u v)) $ model
          setGeometry geom' Forward (u', _) (Just (v', _)) (u, _, v, _, ev)
              | u == u' && v == v' = ev{geometry=geom'}
          setGeometry geom' Reverse (v', _) (Just (u', _)) (u, _, v, _, ev)
              | u == u' && v == v' = ev{geometry=reverse geom'}
          setGeometry _ _ _ _ (_, _, _, _, ev) = ev

moveElasticEdge :: Point -> ViewAction
moveElasticEdge xy' model view = do
    ec <- readIORef $ elasticContext view
    case ec of
        Nothing -> return ()
        Just (ElasticContext model' (ElasticEdge dir u v _)) ->
            startElasticEdge dir u v xy' model' view

endElasticEdge :: Point -> ModelAndViewUpdate (IO ())
endElasticEdge xy' modelRef view = do
    ec <- readIORef $ elasticContext view
    model <- readIORef modelRef
    (endElasticEdge' ec (hitTest xy' $ graph model) <@> clearElasticContext) modelRef view
    where
        endElasticEdge' :: Maybe ElasticContext -> HitTest -> ModelTransition
        endElasticEdge' (Just (ElasticContext m'
                               (ElasticEdge dir u Nothing _)))
                        (OnNode v') =
            addEdge dir u v'
        endElasticEdge' (Just (ElasticContext m'
                               (ElasticEdge dir u@(uId, _) (Just v@(vId, _)) _)))
                        (OnNode v') =
            addEdge dir u v' . deleteEdge dir u v
        endElasticEdge' _ _ = id

        deleteEdge Forward (u, _) (v, _) = liftModel $ delEdge (u, v)
        deleteEdge Reverse (v, _) (u, _) = liftModel $ delEdge (u, v)

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

    layoutMotionNotifyHandler <- newIORef $ const idView
    layoutButtonReleaseHandler <- newIORef $ const $ id <@> idView

    context <- newIORef NoContext

    elasticContext <- newIORef Nothing

    let view = View builder window
               (Menu fileNewItem fileOpenItem fileSaveItem fileSaveAsItem fileQuitItem
                editCutItem editCopyItem editPasteItem editDeleteItem helpAboutItem
                nodeDeleteItem edgeDeleteItem)
               nodePopupMenu edgePopupMenu layout statusBar
               layoutMotionNotifyHandler layoutButtonReleaseHandler context
               elasticContext

    return view

renderView :: DrawWindow -> ViewAction
renderView drawWindow model view = renderWithDrawable drawWindow $ do
    setSourceRGB 0 0 0
    ec <- liftIO $ readIORef (elasticContext view)
    let graph' = graph $ maybe model elasticModel ec
    renderNodes $ labNodes graph'
    renderEdges $ labEdges graph'
    renderElasticEdge ec

    where elasticModel :: ElasticContext -> Model
          elasticModel (ElasticContext m _) = m

          renderNodes :: [Node] -> Render ()
          renderNodes = mapM_ renderNode

          renderNode :: Node -> Render ()
          renderNode (_, NodeView (x,y) _) = do
              arc x y nodeRadius 0 $ 2*pi
              stroke

          renderEdges :: [Edge] -> Render ()
          renderEdges = mapM_ renderEdge

          renderEdge :: Edge -> Render ()
          renderEdge (_, _, EdgeView g _ _ _) = renderLineString g

          renderElasticEdge :: Maybe ElasticContext -> Render ()
          renderElasticEdge (Just (ElasticContext _ (ElasticEdge _ _ Nothing g))) =
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

popupNodeMenu :: Node -> Maybe (MouseButton, TimeStamp) -> ViewAction
popupNodeMenu u bt m v = do
    writeIORef' (context v) $ NodeContext u
    menuPopup (nodePopupMenu v) bt

popupEdgeMenu :: Edge -> Maybe (MouseButton, TimeStamp) -> ViewAction
popupEdgeMenu e bt m v = do
    writeIORef' (context v) $ EdgeContext e
    menuPopup (edgePopupMenu v) bt

setDragHandlers :: (Point -> ViewAction)
                   -> (Point -> ModelAndViewUpdate (IO ()))
                   -> ViewAction
setDragHandlers dragging released _ view = do
    writeIORef' (layoutMotionNotifyHandler view) dragging
    writeIORef' (layoutButtonReleaseHandler view) released

clearDragHandlers :: ViewAction
clearDragHandlers _ view = do
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
        model <- readIORef m
        pressed click button (Just (button, timestamp)) xy (hitTest xy $ graph model) m v
    return True
    where
        pressed :: Click -> MouseButton -> Maybe (MouseButton, TimeStamp)
                   -> (Double, Double) -> HitTest
                   -> ModelAndViewUpdate (IO ())
        pressed SingleClick LeftButton _ xy Nowhere = addNewNode xy
        pressed SingleClick LeftButton _ xy (OnNode n) = dragEdge Forward n Nothing xy
        pressed SingleClick MiddleButton _ xy (OnNode n) = dragNode n xy
        pressed SingleClick MiddleButton _ xy (OnEdgeStart (u, _, v)) =
            dragEdge Reverse v (Just u) xy
        pressed SingleClick MiddleButton _ xy (OnEdgeEnd (u, _, v)) =
            dragEdge Forward u (Just v) xy
        pressed SingleClick RightButton bt _ (OnNode n) = id <@> popupNodeMenu n bt
        pressed SingleClick RightButton bt _ (OnEdge e) = id <@> popupEdgeMenu e bt
        pressed SingleClick RightButton bt _ (OnEdgeStart (_, e, _)) =
            id <@> popupEdgeMenu e bt
        pressed SingleClick RightButton bt _ (OnEdgeEnd (_, e, _)) =
            id <@> popupEdgeMenu e bt
        pressed _ _ _ _ _ = \_ _ -> return ()

layoutMotionNotify :: ModelAndViewUpdate (EventM EMotion Bool)
layoutMotionNotify m v = do
    xy <- eventCoordinates
    liftIO $ do
        handler <- readIORef $ layoutMotionNotifyHandler v
        (id <@> handler xy >&> refreshView >&> showInStatus (hitTest xy . graph)) m v
    return True

layoutButtonReleased :: ModelAndViewUpdate (EventM EButton Bool)
layoutButtonReleased m v = do
    xy <- eventCoordinates
    liftIO $ do
        handler <- readIORef $ layoutButtonReleaseHandler v
        (handler xy >>> id <@> refreshView >&> clearDragHandlers) m v
    return True

addNewNode :: Point -> ModelAndViewUpdate (IO ())
addNewNode xy mref v = do
    model <- readIORef mref
    let [n] = newNodes 1 $ graph model
        node = (n, NodeView xy "")
    (liftModel (insNode node) <@> refreshView) mref v
    dragNode node xy mref v

dragNode :: Node -> Point -> ModelAndViewUpdate (IO ())
dragNode n xy = id
                <@> startElasticNode n xy
                >&> refreshView
                >&> setDragHandlers moveElasticNode endElasticNode

dragEdge :: Direction -> Node -> Maybe Node -> Point -> ModelAndViewUpdate (IO ())
dragEdge dir fixedNode otherNode xy = id
                                      <@> startElasticEdge dir fixedNode otherNode xy
                                      >&> refreshView
                                      >&> setDragHandlers moveElasticEdge endElasticEdge

deleteActivated :: ModelAndViewUpdate (IO ())
deleteActivated m v = do
    ctx <- readIORef (context v)
    (delete' ctx <@> refreshView) m v
    where
        delete' (NodeContext n) = deleteNode n
        delete' (EdgeContext e) = deleteEdge e
        delete' _ = id

main :: IO ()
main = do
    initGUI
    view <- createView
    controller initialModel view
    mainGUI

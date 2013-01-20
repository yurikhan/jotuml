{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
import Control.Exception.Base
import Control.Monad
import qualified Control.Monad.State.Lazy as S
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

import Model


trace' :: Show a => a -> a
trace' x = traceShow x x


{- The Model -}

type ModelRef = IORef Model

type Reference = LEdge ()

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

type ViewAction = Model -> View -> IO ()

idView :: ViewAction
idView _ _ = return ()

clearElasticContext :: ViewAction
clearElasticContext model view = do
    writeIORef' (elasticContext view) Nothing

startElasticNode :: Element -> Point -> ViewAction
startElasticNode u@(uId, NodeLabel _) xy' model view = do
    writeIORef' (elasticContext view) $ Just $ ElasticContext m'
        $ ElasticNode (uId, NodeLabel xy')
    where m' = S.execState (moveNode u xy') model

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
        endElasticNode' Nothing = idModel
        endElasticNode' (Just (ElasticContext model' (ElasticNode u))) = moveNode u xy'

startElasticEdge :: Direction -> Element -> Maybe Element -> Maybe Element -> Point -> ViewAction
startElasticEdge dir u@(_, NodeLabel xy) e v xy' model view = do
    writeIORef' (elasticContext view) $
        Just $ ElasticContext model' $ ElasticEdge dir u e v $ geom' e
    where geom' Nothing = makeGeometry (xy, 10) (xy', 0)
          geom' (Just (_, EdgeLabel geom)) = liftG dir ((++ [xy']) . init) geom
          model' = S.execState (maybe idModel deleteEdge e) model

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
    model <- readIORef modelRef
    (release' ec (hitTest model xy')) modelRef view
    where
        release' :: Maybe ElasticContext -> HitTest -> ModelAndViewUpdate (IO ())
        release' (Just (ElasticContext _ (ElasticEdge dir u Nothing Nothing g)))
                 (Just v'@(_, NodeLabel _)) =
            addEdge dir u v' g <@> clearElasticContext >&> clearDragHandlers
        release' (Just (ElasticContext _ (ElasticEdge dir u (Just e) (Just v) g)))
                 (Just v'@(_, NodeLabel _)) =
            (rerouteEdge e g >> reconnectEdge e dir v')
            <@> clearElasticContext >&> clearDragHandlers
        release' (Just (ElasticContext _ (ElasticEdge _ _ _ _ _))) _ =
            idModel <@> modifyElasticEdgeGeometry (++ [xy'])
        release' _ _ = idModel <@> clearElasticContext >&> clearDragHandlers

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
    layoutButtonReleaseHandler <- newIORef $ const $ idModel <@> idView

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
    let model' = maybe model elasticModel ec
    renderNodes $ modelNodes model'
    renderEdges $ modelEdges model'
    renderElasticEdge ec

    where elasticModel :: ElasticContext -> Model
          elasticModel (ElasticContext m _) = m

          renderNodes :: [Element] -> Render ()
          renderNodes = mapM_ renderNode

          renderNode :: Element -> Render ()
          renderNode (_, NodeLabel (x,y)) = do
              arc x y nodeRadius 0 $ 2*pi
              stroke

          renderEdges :: [Element] -> Render ()
          renderEdges = mapM_ renderEdge

          renderEdge :: Element -> Render ()
          renderEdge (_, EdgeLabel g) = renderLineString g

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
    writeIORef' (layoutButtonReleaseHandler view) $ const $ idModel <@> idView

{- The Controller -}

controller :: Model -> View -> IO ()
controller m v = do
    modelRef <- newIORef m
    setupView modelRef v
    refreshView m v

type ModelAndViewUpdate r = ModelRef -> View -> r

(<@>) :: ModelAction a -> ViewAction -> ModelAndViewUpdate (IO ())
infix 6 <@>
modelAction <@> viewAction = \ modelRef view -> do
    model <- readIORef modelRef
    let model' = S.execState modelAction model
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
    liftIO $ (idModel <@> renderView drawWindow) m v
    return True

layoutButtonPressed :: ModelAndViewUpdate (EventM EButton Bool)
layoutButtonPressed m v = do
    button <- eventButton
    timestamp <- eventTime
    click <- eventClick
    xy <- eventCoordinates
    liftIO $ do
        model <- readIORef m
        handler <- readIORef $ layoutButtonPressHandler v
        handler click button (Just (button, timestamp)) xy (hitTest model xy) m v
    return True

pressed :: Click -> MouseButton -> Maybe (MouseButton, TimeStamp)
           -> (Double, Double) -> HitTest
           -> ModelAndViewUpdate (IO ())
pressed SingleClick LeftButton _ xy Nothing = addNewNode xy
pressed SingleClick LeftButton _ xy (Just n@(_, NodeLabel _)) =
    dragEdge Forward n Nothing Nothing xy
pressed SingleClick MiddleButton _ xy (Just n@(_, NodeLabel _)) = dragNode n xy
pressed SingleClick MiddleButton _ xy (Just end@(_, EdgeEndLabel dir)) = \mref view -> do
    m <- readIORef mref
    dragEdge (dirReverse dir) (endNode m $ endOther m end)
        (Just $ endEdge m end) (Just $ endNode m end) xy mref view
pressed SingleClick RightButton bt _ (Just n@(_, NodeLabel _)) =
    idModel <@> popupNodeMenu n bt
pressed SingleClick RightButton bt _ (Just e@(_, EdgeLabel _)) =
    idModel <@> popupEdgeMenu e bt
pressed SingleClick RightButton bt _ (Just end@(_, EdgeEndLabel _)) = \mref view -> do
    m <- readIORef mref
    (idModel <@> popupEdgeMenu (endEdge m end) bt) mref view
pressed _ _ _ _ _ = idModel <@> idView
ignorePressed _ _ _ _ _ = idModel <@> idView

layoutMotionNotify :: ModelAndViewUpdate (EventM EMotion Bool)
layoutMotionNotify m v = do
    xy <- eventCoordinates
    liftIO $ do
        handler <- readIORef $ layoutMotionNotifyHandler v
        (idModel <@> handler xy >&> refreshView >&> showInStatus (`hitTest` xy)) m v
    return True

layoutButtonReleased :: ModelAndViewUpdate (EventM EButton Bool)
layoutButtonReleased m v = do
    xy <- eventCoordinates
    liftIO $ do
        handler <- readIORef $ layoutButtonReleaseHandler v
        (handler xy >>> idModel <@> refreshView) m v
    return True

addNewNode :: Point -> ModelAndViewUpdate (IO ())
addNewNode xy = addNode xy <@> refreshView
                >>> \mref view -> do
                    m <- readIORef mref
                    dragNode (fromJust $ hitTest m xy) xy mref view

dragNode :: Element -> Point -> ModelAndViewUpdate (IO ())
dragNode n xy = idModel
                <@> startElasticNode n xy
                >&> refreshView
                >&> setDragHandlers moveElasticNode endElasticNode

dragEdge :: Direction -> Element -> Maybe Element -> Maybe Element -> Point
            -> ModelAndViewUpdate (IO ())
dragEdge dir fixedNode edge otherNode xy =
    idModel
    <@> startElasticEdge dir fixedNode edge otherNode xy
    >&> refreshView
    >&> setDragHandlers moveElasticEdge releaseElasticEdge

deleteActivated :: ModelAndViewUpdate (IO ())
deleteActivated m v = do
    ctx <- readIORef (context v)
    (delete' ctx <@> refreshView) m v
    where
        delete' (NodeContext n) = deleteNode n
        delete' (EdgeContext e) = deleteEdge e
        delete' _ = idModel

dumpGraph :: ModelAndViewUpdate (IO ())
dumpGraph = idModel <@> dumpGraph'
    where dumpGraph' = flip $ const print

main :: IO ()
main = do
    initGUI
    view <- createView
    controller star view
    mainGUI

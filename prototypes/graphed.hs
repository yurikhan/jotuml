{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ExistentialQuantification #-}
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
             | NodeContext Node
             | forall edge . EdgeElement edge => EdgeContext edge

data ElasticContext = ElasticContext Model ElasticElement

data ElasticElement = forall a . PointElement a =>
                      ElasticPoint a
                    | ElasticPath Direction
                      LineString
                      (LineString -> ModelAction ())
                      ElasticPathContext
                    | forall a . LinearElement a =>
                      ElasticBend a Int
                      (Int -> Point -> LineString -> LineString)

data ElasticPathContext = FromNode Node
                        | FromEdgeEnd
                          Node -- fixed node
                          EdgeEnd -- fixed end
                          Edge
                          EdgeEnd -- moving end
                          Node -- moving node
                        | FromEdgeBend Edge Int
                        | FromEdgeSegment Edge Int
                        | FromEdgeDiamond EdgeDiamond
                        | FromBranchStart
                          EdgeDiamond -- donor
                          EdgeBranch -- breakoff
                          Node -- fixed node
                        | FromBranchEnd
                          EdgeDiamond -- fixed edge
                          EdgeBranch -- breakoff
                          Node -- donor

isDangling :: ElasticPathContext -> Bool
isDangling (FromNode _)          = True
isDangling (FromEdgeBend _ _)    = True
isDangling (FromEdgeSegment _ _) = True
isDangling (FromEdgeDiamond _)   = True
isDangling _                     = False

type ViewAction = Model -> View -> IO ()

idView :: ViewAction
idView _ _ = return ()

clearElasticContext :: ViewAction
clearElasticContext model view = do
    writeIORef' (elasticContext view) Nothing

startElasticBend :: LinearElement a
                    => (Int -> Point -> LineString -> LineString)
                    -> a -> Int -> Point -> ViewAction
startElasticBend f e i xy' model view = do
    writeIORef' (elasticContext view) $ Just $ ElasticContext m' $ ElasticBend e i f
    where m' = S.execState (rerouteGeometry e $ f i xy' $ getGeometry model e) model

moveElasticBend :: Point -> ViewAction
moveElasticBend xy' model view = do
    ec <- readIORef $ elasticContext view
    case ec of
        Just (ElasticContext model' (ElasticBend e i f)) ->
            startElasticBend f e i xy' model view
        _ -> return ()

releaseElasticBend :: Point -> ModelAndViewUpdate (IO ())
releaseElasticBend xy' modelRef view = do
    Just (ElasticContext m' ee) <- readIORef $ elasticContext view
    (release m' ee <@> clearElasticContext >&> clearDragHandlers) modelRef view
    where
        release m (ElasticBend e i f) = rerouteGeometry e
                                        $ normalizeBend i
                                        $ f i xy'
                                        $ getGeometry m e


startElasticPoint :: PointElement a => a -> Point -> ViewAction
startElasticPoint u xy' model view = do
    writeIORef' (elasticContext view) $ Just $ ElasticContext m' $ ElasticPoint u
    where m' = S.execState (moveXY u xy') model

moveElasticPoint :: Point -> ViewAction
moveElasticPoint xy' _ view = do
    ec <- readIORef $ elasticContext view
    case ec of
        Just (ElasticContext model' (ElasticPoint u)) -> startElasticPoint u xy' model' view
        _ -> return ()

endElasticPoint :: Point -> ModelAndViewUpdate (IO ())
endElasticPoint xy' modelRef view = do
    ec <- readIORef $ elasticContext view
    (endElasticPoint' ec <@> clearElasticContext >&> clearDragHandlers) modelRef view
    where
        endElasticPoint' Nothing = idModel
        endElasticPoint' (Just (ElasticContext model' (ElasticPoint u))) = moveXY u xy'


startElasticPath :: Direction
                    -> LineString -- initial geometry
                    -> (LineString -> ModelAction ()) -- update tmp model with new geometry
                    -> ElasticPathContext
                    -> ViewAction
startElasticPath dir geom' setGeom elasticPathContext model view = do
    writeIORef' (elasticContext view) $ Just
        $ ElasticContext (S.execState (setGeom geom') model)
        $ ElasticPath dir geom' setGeom
        $ elasticPathContext

startElasticPathFromNode :: Node -> Point -> ViewAction
startElasticPathFromNode u xy' model view = do
    startElasticPath Forward geom' (const idModel) (FromNode u) model view
    where geom' = makeGeometry (uxy, 10) (xy', 0)
          uxy = nodeXY model u

startElasticPathFromEdgeEnd :: EdgeEnd -> Point -> ViewAction
startElasticPathFromEdgeEnd movingEnd xy' model view =
    startElasticPath fixedDir geom' (setEdgeGeometry e)
        (FromEdgeEnd fixedNode fixedEnd e movingEnd movingNode)
        model view
    where geom' = liftG fixedDir (snapGeometry (fixedXY, nodeRadius) (xy', 0)) geom
          geom = edgeGeometry model e
          fixedDir = endDirection model fixedEnd
          fixedNode = endNode model fixedEnd
          fixedXY = nodeXY model fixedNode
          fixedEnd = endOther model movingEnd
          e = endEdge model movingEnd
          movingNode = endNode model movingEnd

startElasticPathFromEdge :: Point -> ElasticPathContext -> Edge -> Point -> ViewAction
startElasticPathFromEdge exy elasticPathContext e xy' =
    startElasticPath Forward geom' (const $ splitEdge' e exy >> return ()) elasticPathContext
    where geom' = makeGeometry (exy, 10) (xy', 0)


startElasticPathFromEdgeBend :: Edge -> Int -> Point -> ViewAction
startElasticPathFromEdgeBend e i xy' model view =
    startElasticPathFromEdge (edgeGeometry model e !! i) (FromEdgeBend e i) e xy' model view

startElasticPathFromEdgeSegment :: Edge -> Int -> Point -> ViewAction
startElasticPathFromEdgeSegment e i xy' =
    startElasticPathFromEdge (projection xy' e) (FromEdgeSegment e i) e xy'

startElasticPathFromEdgeDiamond :: EdgeDiamond -> Point -> ViewAction
startElasticPathFromEdgeDiamond d xy' model view =
    startElasticPath Forward geom' (const idModel) (FromEdgeDiamond d) model view
    where geom' = makeGeometry (exy, 10) (xy', 0)
          exy = diamondXY model d

startElasticPathFromBranchTip :: PointElement a
                                 => Direction
                                 -> (EdgeDiamond -> EdgeBranch -> Node -> ElasticPathContext)
                                 -> (Model -> EdgeBranch -> a)
                                 -> EdgeBranch -> Point -> ViewAction
startElasticPathFromBranchTip dir tipType fixed b xy' model view =
    startElasticPath dir geom' (setBranchGeometry b)
        (tipType d b w)
        model view
    where geom' = liftG dir (snapGeometry (fxy, 10) (xy', 0)) geom
          geom = branchGeometry model b
          d = branchDiamond model b
          w = branchNode model b
          fxy = getXY model $ fixed model b

startElasticPathFromBranchStart :: EdgeBranch -> Point -> ViewAction
startElasticPathFromBranchStart =
    startElasticPathFromBranchTip Reverse FromBranchStart branchNode

startElasticPathFromBranchEnd :: EdgeBranch -> Point -> ViewAction
startElasticPathFromBranchEnd =
    startElasticPathFromBranchTip Forward FromBranchEnd branchDiamond

modifyElasticPathGeometry :: (LineString -> LineString) -> ViewAction
modifyElasticPathGeometry f model view = do
    ec <- readIORef $ elasticContext view
    modify' ec
    where modify' Nothing = return ()
          modify' (Just (ElasticContext _ (ElasticPath dir geom setGeom elasticPathContext))) =
              writeIORef' (elasticContext view) $ Just
                  $ ElasticContext (S.execState (setGeom geom') model)
                  $ ElasticPath dir geom' setGeom elasticPathContext
                  where geom' = liftG dir f geom

moveElasticPath :: Point -> ViewAction
moveElasticPath xy' = modifyElasticPathGeometry ((++ [xy']) . init)

releaseElasticPath :: Point -> ModelAndViewUpdate (IO ())
releaseElasticPath xy' modelRef view = do
    ec <- readIORef $ elasticContext view
    case ec of
        Nothing -> (idModel <@> endDrag) modelRef view
        Just (ElasticContext _ ep) -> do
            model <- readIORef modelRef
            (release' ep (hitTest model xy')) modelRef view
    where
        endDrag = clearElasticContext >&> clearDragHandlers

        release' :: ElasticElement -> HitTest -> ModelAndViewUpdate (IO ())

        release' (ElasticPath dir g _ (FromNode u)) (OnNode v') =
            addEdge dir u v' g <@> endDrag

        release' (ElasticPath dir g _ (FromNode u)) (OnEdgeDiamond d) =
            addBranch d u (reverse g) <@> endDrag
        release' (ElasticPath dir g _ (FromEdgeDiamond d)) (OnNode w) =
            addBranch d w g <@> endDrag
        release' (ElasticPath dir g _ (FromNode u)) (OnEdgeSegment e _) =
            splitEdge e addBranch u (reverse g) <@> endDrag
        release' (ElasticPath dir g _ (FromEdgeSegment e _)) (OnNode w) =
            splitEdge e addBranch w g <@> endDrag
        release' (ElasticPath dir g _ (FromNode u)) (OnEdgeBend e i) =
            splitAtBend e i addBranch u (reverse g) <@> endDrag
        release' (ElasticPath dir g _ (FromEdgeBend e i)) (OnNode w) =
            splitAtBend e i addBranch w g <@> endDrag

        release' (ElasticPath dir g _ (FromEdgeEnd _ _ _ me _)) (OnNode m') =
            reconnectEdgeEnd me m' g <@> endDrag
        release' (ElasticPath dir g _ (FromBranchEnd _ we _)) (OnNode w') =
            reconnectBranchNode we w' g <@> endDrag

        release' (ElasticPath dir g _ (FromEdgeEnd _ fe _ _ _)) (OnEdgeDiamond e') =
            makeBranchFromEdge e' fe (dirReverse dir ^$^ g) <@> endDrag
        release' (ElasticPath dir g _ (FromEdgeEnd _ fe _ _ _)) (OnEdgeSegment e' _) =
            splitEdge e' makeBranchFromEdge fe (dirReverse dir ^$^ g) <@> endDrag
        release' (ElasticPath dir g _ (FromEdgeEnd _ fe _ _ _)) (OnEdgeBend e' i) =
            splitAtBend e' i makeBranchFromEdge fe (dirReverse dir ^$^ g) <@> endDrag

        release' (ElasticPath dir g _ (FromBranchStart _ we _)) (OnNode u') =
            makeEdgeFromBranch we u' g <@> endDrag

        release' (ElasticPath dir g _ (FromBranchStart _ we _)) (OnEdgeDiamond e') =
            reconnectBranchDiamond e' we g <@> endDrag
        release' (ElasticPath dir g _ (FromBranchStart _ we _)) (OnEdgeSegment e' _) =
            splitEdge e' reconnectBranchDiamond we g <@> endDrag
        release' (ElasticPath dir g _ (FromBranchStart _ we _)) (OnEdgeBend e' i) =
            splitAtBend e' i reconnectBranchDiamond we g <@> endDrag

        release' (ElasticPath _ _ _ _) _ = idModel <@> modifyElasticPathGeometry (++ [xy'])

        release' _ _ = idModel <@> endDrag


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

    [nodePopupMenu, edgePopupMenu] <-
        builderGetObject builder castToMenu
        `mapM` ["node_popup_menu", "edge_popup_menu"]

    [fileNewItem, fileOpenItem, fileSaveItem, fileSaveAsItem, fileQuitItem,
     editCutItem, editCopyItem, editPasteItem, editDeleteItem,
     helpAboutItem, nodeDeleteItem, edgeDeleteItem] <-
        builderGetObject builder castToMenuItem
        `mapM` ["file_new_item", "file_open_item", "file_save_item",
                "file_save_as_item", "file_quit_item",
                "edit_cut_item", "edit_copy_item", "edit_paste_item", "edit_delete_item",
                "help_about_item", "node_delete_item", "edge_delete_item"]

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
    mapM_ renderNode $ modelNodes model'
    mapM_ renderEdge $ modelEdges model'
    mapM_ renderDiamond $ modelEdgeDiamonds model'
    mapM_ renderBranch $ modelEdgeBranches model'
    renderElasticEdge ec

    where elasticModel :: ElasticContext -> Model
          elasticModel (ElasticContext m _) = m

          renderNode :: Node -> Render ()
          renderNode n = do
              let (x, y) = nodeXY model n
              arc x y nodeRadius 0 $ 2*pi
              stroke

          renderEdge :: Edge -> Render ()
          renderEdge e = renderLineString $ edgeGeometry model e

          renderDiamond :: EdgeDiamond -> Render ()
          renderDiamond d = renderLineString $ moveGeometry (diamondXY model d)
                            [(nodeRadius, 0), (0, (-nodeRadius)),
                             ((-nodeRadius), 0), (0, nodeRadius),
                             (nodeRadius, 0)]

          renderBranch :: EdgeBranch -> Render ()
          renderBranch b = renderLineString $ branchGeometry model b

          renderElasticEdge :: Maybe ElasticContext -> Render ()
          renderElasticEdge (Just (ElasticContext _ (ElasticPath _ g _ ctx)))
              | isDangling ctx = renderLineString g
          renderElasticEdge _ = return ()

          renderLineString :: LineString -> Render ()
          renderLineString [] = return ()
          renderLineString g@((x0,y0):xys) = do
              moveTo x0 y0
              mapM_ (uncurry lineTo) xys
              renderArrowhead 10 (pi/6) xyN1 xyN
              stroke
              where xyN : xyN1 : _ = reverse g

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

popupEdgeMenu :: EdgeElement edge => edge -> Maybe (MouseButton, TimeStamp) -> ViewAction
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
pressed SingleClick LeftButton _ xy Nowhere = addNewNode xy
pressed SingleClick LeftButton _ xy (OnNode n) = dragPath $ startElasticPathFromNode n xy
pressed SingleClick LeftButton _ xy (OnEdgeBend e i) =
    dragPath $ startElasticPathFromEdgeBend e i xy
pressed SingleClick LeftButton _ xy (OnEdgeSegment e i) =
    dragPath $ startElasticPathFromEdgeSegment e i xy
pressed SingleClick LeftButton _ xy (OnEdgeDiamond e) =
    dragPath $ startElasticPathFromEdgeDiamond e xy
pressed SingleClick MiddleButton _ xy (OnNode n) = dragPoint n xy
pressed SingleClick MiddleButton _ xy (OnEdgeDiamond e) = dragPoint e xy
pressed SingleClick MiddleButton _ xy (OnEdgeEnd end) =
    dragPath $ startElasticPathFromEdgeEnd end xy
pressed SingleClick MiddleButton _ xy (OnBranchStart end) =
    dragPath $ startElasticPathFromBranchStart end xy
pressed SingleClick MiddleButton _ xy (OnBranchEnd end) =
    dragPath $ startElasticPathFromBranchEnd end xy
pressed SingleClick MiddleButton _ xy (OnEdgeBend e i) =
    dragBend $ startElasticBend moveBend e i xy
pressed SingleClick MiddleButton _ xy (OnEdgeSegment e i) =
    dragBend $ startElasticBend addBend e (i + 1) xy
pressed SingleClick MiddleButton _ xy (OnBranchBend e i) =
    dragBend $ startElasticBend moveBend e i xy
pressed SingleClick MiddleButton _ xy (OnBranchSegment e i) =
    dragBend $ startElasticBend addBend e (i + 1) xy
pressed SingleClick RightButton bt _ (OnNode n) = idModel <@> popupNodeMenu n bt
pressed SingleClick RightButton bt _ (OnEdgeBend e _) = idModel <@> popupEdgeMenu e bt
pressed SingleClick RightButton bt _ (OnEdgeSegment e _) = idModel <@> popupEdgeMenu e bt
pressed SingleClick RightButton bt _ (OnEdgeEnd end) = \mref view -> do
    m <- readIORef mref
    (idModel <@> popupEdgeMenu (endEdge m end) bt) mref view
pressed SingleClick RightButton bt _ (OnEdgeDiamond d) = idModel <@> popupEdgeMenu d bt
pressed SingleClick RightButton bt _ (OnBranchStart b) = \mref view -> do
    m <- readIORef mref
    (idModel <@> popupEdgeMenu (branchDiamond m b) bt) mref view
pressed SingleClick RightButton bt _ (OnBranchEnd b) = \mref view -> do
    m <- readIORef mref
    (idModel <@> popupEdgeMenu (branchDiamond m b) bt) mref view
pressed SingleClick RightButton bt _ (OnBranchBend b _) = \mref view -> do
    m <- readIORef mref
    (idModel <@> popupEdgeMenu (branchDiamond m b) bt) mref view
pressed SingleClick RightButton bt _ (OnBranchSegment b _) = \mref view -> do
    m <- readIORef mref
    (idModel <@> popupEdgeMenu (branchDiamond m b) bt) mref view
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
                    let OnNode n = hitTest m xy
                    dragPoint n xy mref view

dragPoint :: PointElement a => a -> Point -> ModelAndViewUpdate (IO ())
dragPoint n xy = idModel
                 <@> startElasticPoint n xy
                 >&> refreshView
                 >&> setDragHandlers moveElasticPoint endElasticPoint

dragBend :: ViewAction -> ModelAndViewUpdate (IO ())
dragBend va = idModel <@> va
                      >&> refreshView
                      >&> setDragHandlers moveElasticBend releaseElasticBend

dragPath :: ViewAction -> ModelAndViewUpdate (IO ())
dragPath va = idModel
              <@> va >&> refreshView >&> setDragHandlers moveElasticPath releaseElasticPath

deleteActivated :: ModelAndViewUpdate (IO ())
deleteActivated m v = do
    ctx <- readIORef (context v)
    (delete' ctx <@> refreshView) m v
    where
        delete' (NodeContext n) = deleteNode n
        delete' (EdgeContext e) = delete e
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

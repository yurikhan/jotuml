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
             | EdgeContext Edge
             deriving (Show)

data ElasticContext = ElasticContext Model ElasticElement

data ElasticElement = ElasticNode Node
                    | ElasticDiamond Edge
                    | ElasticPath Direction
                      LineString
                      (LineString -> ModelAction ())
                      ElasticPathContext
                    | ElasticEdgeBend Edge Int
                    | ElasticEdgeNewBend Edge Int
                    | ElasticBranchBend EdgeEnd Int
                    | ElasticBranchNewBend EdgeEnd Int

data ElasticPathContext = FromNode Node
                        | FromEdgeEnd
                          Node -- fixed node
                          EdgeEnd -- fixed end
                          Edge
                          EdgeEnd -- moving end
                          Node -- moving node
                        | FromEdgeBend Edge Int
                        | FromEdgeSegment Edge Int
                        | FromEdgeDiamond Edge
                        | FromBranchStart
                          Edge -- donor
                          EdgeEnd -- breakoff
                          Node -- fixed node
                        | FromBranchEnd
                          Edge -- fixed edge
                          EdgeEnd -- breakoff
                          Node -- donor

type ViewAction = Model -> View -> IO ()

idView :: ViewAction
idView _ _ = return ()

clearElasticContext :: ViewAction
clearElasticContext model view = do
    writeIORef' (elasticContext view) Nothing

startElasticEdgeBend :: Edge -> Int -> Point -> ViewAction
startElasticEdgeBend e i xy' model view = do
    writeIORef' (elasticContext view) $ Just $ ElasticContext m'
        $ ElasticEdgeBend e i
    where m' = S.execState (rerouteEdge e $ moveBend i xy' $ edgeGeometry model e) model

startElasticEdgeNewBend :: Edge -> Int -> Point -> ViewAction
startElasticEdgeNewBend e i xy' model view = do
    writeIORef' (elasticContext view) $ Just $ ElasticContext m'
        $ ElasticEdgeNewBend e i
    where m' = S.execState (rerouteEdge e $ addBend i xy' $ edgeGeometry model e) model

startElasticBranchBend :: EdgeEnd -> Int -> Point -> ViewAction
startElasticBranchBend end i xy' model view = do
    writeIORef' (elasticContext view) $ Just $ ElasticContext m'
        $ ElasticBranchBend end i
    where m' = S.execState (rerouteEdgeEnd end $ moveBend i xy' $ endGeometry model end) model

startElasticBranchNewBend :: EdgeEnd -> Int -> Point -> ViewAction
startElasticBranchNewBend end i xy' model view = do
    writeIORef' (elasticContext view) $ Just $ ElasticContext m'
        $ ElasticBranchNewBend end i
    where m' = S.execState (rerouteEdgeEnd end $ addBend i xy' $ endGeometry model end) model


moveElasticBend :: Point -> ViewAction
moveElasticBend xy' model view = do
    ec <- readIORef $ elasticContext view
    case ec of
        Just (ElasticContext model' (ElasticEdgeBend e i)) ->
            startElasticEdgeBend e i xy' model view
        Just (ElasticContext model' (ElasticEdgeNewBend e i)) ->
            startElasticEdgeNewBend e i xy' model view
        Just (ElasticContext model' (ElasticBranchBend e i)) ->
            startElasticBranchBend e i xy' model view
        Just (ElasticContext model' (ElasticBranchNewBend e i)) ->
            startElasticBranchNewBend e i xy' model view
        _ -> return ()

releaseElasticBend :: Point -> ModelAndViewUpdate (IO ())
releaseElasticBend xy' modelRef view = do
    Just (ElasticContext m' ee) <- readIORef $ elasticContext view
    (release m' ee <@> clearElasticContext >&> clearDragHandlers) modelRef view
    where
        release m (ElasticEdgeBend e i) = rerouteEdge e $ moveBend i xy' $ edgeGeometry m e
        release m (ElasticEdgeNewBend e i) = rerouteEdge e $ addBend i xy' $ edgeGeometry m e
        release m (ElasticBranchBend e i) = rerouteEdgeEnd e $ moveBend i xy' $ endGeometry m e
        release m (ElasticBranchNewBend e i) = rerouteEdgeEnd e $ addBend i xy' $ endGeometry m e


startElasticNode :: Node -> Point -> ViewAction
startElasticNode u xy' model view = do
    writeIORef' (elasticContext view) $ Just $ ElasticContext m'
        $ ElasticNode $ nodeMove u xy'
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

startElasticDiamond :: Edge -> Point -> ViewAction
startElasticDiamond e xy' model view = do
    writeIORef' (elasticContext view) $ Just $ ElasticContext m'
        $ ElasticDiamond $ edgeDiamondMove e xy'
    where m' = S.execState (moveEdgeDiamond e xy') model

moveElasticDiamond :: Point -> ViewAction
moveElasticDiamond xy' _ view = do
    ec <- readIORef $ elasticContext view
    case ec of
        Just (ElasticContext model' (ElasticDiamond e)) -> startElasticDiamond e xy' model' view
        _ -> return ()

endElasticDiamond :: Point -> ModelAndViewUpdate (IO ())
endElasticDiamond xy' modelRef view = do
    ec <- readIORef $ elasticContext view
    (endElasticDiamond' ec <@> clearElasticContext >&> clearDragHandlers) modelRef view
    where
        endElasticDiamond' Nothing = idModel
        endElasticDiamond' (Just (ElasticContext model (ElasticDiamond e))) =
            moveEdgeDiamond e xy'

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
    startElasticPath Forward geom' (const $ splitEdge e exy >> return ()) elasticPathContext
    where geom' = makeGeometry (exy, 10) (xy', 0)

startElasticPathFromEdgeBend :: Edge -> Int -> Point -> ViewAction
startElasticPathFromEdgeBend e i xy' model view =
    startElasticPathFromEdge (edgeGeometry model e !! i) (FromEdgeBend e i) e xy' model view

startElasticPathFromEdgeSegment :: Edge -> Int -> Point -> ViewAction
startElasticPathFromEdgeSegment e i xy' =
    startElasticPathFromEdge (projection xy' e) (FromEdgeSegment e i) e xy'

startElasticPathFromEdgeDiamond :: Edge -> Point -> ViewAction
startElasticPathFromEdgeDiamond e xy' model view =
    startElasticPathFromEdge (edgeDiamondXY model e) (FromEdgeDiamond e) e xy' model view

startElasticPathFromBranchStart :: EdgeEnd -> Point -> ViewAction
startElasticPathFromBranchStart end xy' model view =
    startElasticPath Reverse geom' (setEdgeEndGeometry end)
        (FromBranchStart e end w)
        model view
    where geom' = snapGeometry (xy', 0) (wxy, 10) geom
          geom = endGeometry model end
          e = endEdge model end
          w = endNode model end
          wxy = nodeXY model w

startElasticPathFromBranchEnd :: EdgeEnd -> Point -> ViewAction
startElasticPathFromBranchEnd end xy' model view =
    startElasticPath Forward geom' (setEdgeEndGeometry end)
        (FromBranchEnd e end w)
        model view
    where geom' = snapGeometry (exy, 10) (xy', 0) geom
          geom = endGeometry model end
          e = endEdge model end
          w = endNode model end
          exy = edgeDiamondXY model e

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
        release' (ElasticPath dir g _ (FromNode u)) (OnEdgeBend e i) =
            addEdgeEndAtBend e i u (reverse g) <@> endDrag
        release' (ElasticPath dir g _ (FromNode u)) (OnEdgeSegment e _) =
            addEdgeEnd e u (reverse g) <@> endDrag
        release' (ElasticPath dir g _ (FromNode u)) (OnEdgeDiamond e) =
            addEdgeEnd e u (reverse g) <@> endDrag
        release' (ElasticPath dir g _ (FromEdgeBend e i)) (OnNode w) =
            addEdgeEndAtBend e i w g <@> endDrag
        release' (ElasticPath dir g _ (FromEdgeSegment e _)) (OnNode w) =
            addEdgeEnd e w g <@> endDrag
        release' (ElasticPath dir g _ (FromEdgeDiamond e)) (OnNode w) =
            addEdgeEnd e w g <@> endDrag
        release' (ElasticPath dir g _ (FromEdgeEnd _ _ _ me _)) (OnNode m') =
            reconnectEdgeEnd me m' g <@> endDrag
        release' (ElasticPath dir g _ (FromBranchEnd _ we _)) (OnNode w') =
            reconnectEdgeEnd we w' g <@> endDrag
        release' (ElasticPath dir g _ (FromEdgeEnd _ fe _ _ _)) (OnEdgeBend e' i) =
            makeBranchFromEdgeAtBend fe e' i (dirReverse dir ^$^ g) <@> endDrag
        release' (ElasticPath dir g _ (FromEdgeEnd _ fe _ _ _)) (OnEdgeSegment e' _) =
            makeBranchFromEdge fe e' (dirReverse dir ^$^ g) <@> endDrag
        release' (ElasticPath dir g _ (FromEdgeEnd _ fe _ _ _)) (OnEdgeDiamond e') =
            makeBranchFromEdge fe e' (dirReverse dir ^$^ g) <@> endDrag
        release' (ElasticPath dir g _ (FromBranchStart _ we _)) (OnNode u') =
            makeEdgeFromBranch we u' g <@> endDrag
        release' (ElasticPath dir g _ (FromBranchStart _ we _)) (OnEdgeBend e' i) =
            reconnectBranchAtBend we e' i g <@> endDrag
        release' (ElasticPath dir g _ (FromBranchStart _ we _)) (OnEdgeSegment e' _) =
            reconnectBranch we e' g <@> endDrag
        release' (ElasticPath dir g _ (FromBranchStart _ we _)) (OnEdgeDiamond e') =
            reconnectBranch we e' g <@> endDrag
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
    renderBranches $ modelEdgeEnds model'
    renderElasticEdge ec

    where elasticModel :: ElasticContext -> Model
          elasticModel (ElasticContext m _) = m

          renderNodes :: [Node] -> Render ()
          renderNodes = mapM_ renderNode

          renderNode :: Node -> Render ()
          renderNode n = do
              let (x, y) = nodeXY model n
              arc x y nodeRadius 0 $ 2*pi
              stroke

          renderEdges :: [Edge] -> Render ()
          renderEdges = mapM_ renderEdge

          renderEdge :: Edge -> Render ()
          renderEdge e
              | edgeIsHyper model e = renderDiamond $ edgeDiamondXY model e
              | otherwise           = renderLineString $ edgeGeometry model e

          renderDiamond :: Point -> Render ()
          renderDiamond xy = renderLineString $ moveGeometry xy
                             [(nodeRadius, 0), (0, (-nodeRadius)),
                              ((-nodeRadius), 0), (0, nodeRadius),
                              (nodeRadius, 0)]

          renderBranches :: [EdgeEnd] -> Render ()
          renderBranches = mapM_ renderBranch

          renderBranch :: EdgeEnd -> Render ()
          renderBranch end
              | endIsHyper model end = renderLineString $ endGeometry model end
              | otherwise = return ()

          renderElasticEdge :: Maybe ElasticContext -> Render ()
          renderElasticEdge (Just (ElasticContext _ (ElasticPath _ g _ (FromNode _)))) =
              renderLineString g
          renderElasticEdge (Just (ElasticContext _ (ElasticPath _ g _ (FromEdgeBend _ _)))) =
              renderLineString g
          renderElasticEdge (Just (ElasticContext _ (ElasticPath _ g _
                                                     (FromEdgeSegment _ _)))) =
              renderLineString g
          renderElasticEdge (Just (ElasticContext _ (ElasticPath _ g _ (FromEdgeDiamond _)))) =
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
pressed SingleClick MiddleButton _ xy (OnNode n) = dragNode n xy
pressed SingleClick MiddleButton _ xy (OnEdgeDiamond e) = dragEdgeDiamond e xy
pressed SingleClick MiddleButton _ xy (OnEdgeEnd end) =
    dragPath $ startElasticPathFromEdgeEnd end xy
pressed SingleClick MiddleButton _ xy (OnBranchStart end) =
    dragPath $ startElasticPathFromBranchStart end xy
pressed SingleClick MiddleButton _ xy (OnBranchEnd end) =
    dragPath $ startElasticPathFromBranchEnd end xy
pressed SingleClick MiddleButton _ xy (OnEdgeBend e i) = dragBend $ startElasticEdgeBend e i xy
pressed SingleClick MiddleButton _ xy (OnEdgeSegment e i) =
    dragBend $ startElasticEdgeNewBend e i xy
pressed SingleClick MiddleButton _ xy (OnBranchBend e i) = dragBend $ startElasticBranchBend e i xy
pressed SingleClick MiddleButton _ xy (OnBranchSegment e i) = dragBend $ startElasticBranchNewBend e i xy
pressed SingleClick RightButton bt _ (OnNode n) = idModel <@> popupNodeMenu n bt
pressed SingleClick RightButton bt _ (OnEdgeBend e _) = idModel <@> popupEdgeMenu e bt
pressed SingleClick RightButton bt _ (OnEdgeSegment e _) = idModel <@> popupEdgeMenu e bt
pressed SingleClick RightButton bt _ (OnEdgeEnd end) = \mref view -> do
    m <- readIORef mref
    (idModel <@> popupEdgeMenu (endEdge m end) bt) mref view
pressed SingleClick RightButton bt _ (OnEdgeDiamond e) = idModel <@> popupEdgeMenu e bt
pressed SingleClick RightButton bt _ (OnBranchStart end) = \mref view -> do
    m <- readIORef mref
    (idModel <@> popupEdgeMenu (endEdge m end) bt) mref view
pressed SingleClick RightButton bt _ (OnBranchEnd end) = \mref view -> do
    m <- readIORef mref
    (idModel <@> popupEdgeMenu (endEdge m end) bt) mref view
pressed SingleClick RightButton bt _ (OnBranchBend end _) = \mref view -> do
    m <- readIORef mref
    (idModel <@> popupEdgeMenu (endEdge m end) bt) mref view
pressed SingleClick RightButton bt _ (OnBranchSegment end _) = \mref view -> do
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
                    let OnNode n = hitTest m xy
                    dragNode n xy mref view

dragNode :: Node -> Point -> ModelAndViewUpdate (IO ())
dragNode n xy = idModel
                <@> startElasticNode n xy
                >&> refreshView
                >&> setDragHandlers moveElasticNode endElasticNode

dragEdgeDiamond :: Edge -> Point -> ModelAndViewUpdate (IO ())
dragEdgeDiamond e xy = idModel
                       <@> startElasticDiamond e xy
                       >&> refreshView
                       >&> setDragHandlers moveElasticDiamond endElasticDiamond

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

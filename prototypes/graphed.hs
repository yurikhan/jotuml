{-# LANGUAGE TypeSynonymInstances,
             FlexibleInstances,
             ExistentialQuantification #-}
import Control.Arrow ((***))
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
import Graphics.UI.Gtk hiding (Menu, Point, Size)
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
    textview :: Gtk.TextView,
    statusBar :: Gtk.Statusbar,
    layoutButtonPressHandler :: IORef (Click
                                       -> MouseButton
                                       -> Maybe (MouseButton, TimeStamp)
                                       -> (Double, Double)
                                       -> HitTest
                                       -> ModelAndViewUpdate (IO ())),
    layoutMotionNotifyHandler :: IORef (Point -> ViewAction),
    layoutButtonReleaseHandler :: IORef (Point -> ModelAndViewUpdate (IO ())),
    context :: IORef Context,
    elasticContext :: IORef (Maybe ElasticContext),
    editContext :: IORef (Maybe EditContext)
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

data EditContext = EditNode Node

data ElasticContext = ElasticContext
                      Model -- temporary, not necessarily canonical form
                      ElasticElement

data ElasticElement = forall a . PointElement a =>
                      ElasticPoint a
                    | ElasticPath Direction
                      LineString -- bends
                      Point -- mouse position
                      ElasticPathContext
                    | forall a . LinearElement a =>
                      ElasticBend a Int
                      (Int -> Point -> LineString -> LineString) -- update bends

data ElasticPathContext = FromNode Node
                        | FromEdgeEnd
                          Node -- fixed node
                          EdgeEnd -- fixed end
                          Edge
                          EdgeEnd -- moving end
                          Node -- moving node
                        | FromEdgeBend Edge Int EdgeDiamond
                        | FromEdgeSegment Edge Int EdgeDiamond
                        | FromEdgeDiamond EdgeDiamond
                        | FromBranchStart
                          EdgeDiamond -- donor
                          EdgeBranch -- breakoff
                          Node -- fixed node
                        | FromBranchEnd
                          EdgeDiamond -- fixed edge
                          EdgeBranch -- breakoff
                          Node -- donor

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
    writeIORef' (elasticContext view)
        . Just
        . ElasticContext m'
        $ ElasticBend e i f
    where m' = S.execState (rerouteGeometry e
                            . f i xy'
                            $ getGeometry model e) model

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
    writeIORef' (elasticContext view)
        . Just 
        . ElasticContext m'
        $ ElasticPoint u
    where m' = S.execState (moveXY u xy') model

moveElasticPoint :: Point -> ViewAction
moveElasticPoint xy' model view = do
    ec <- readIORef $ elasticContext view
    case ec of
        Just (ElasticContext model' (ElasticPoint u)) ->
            startElasticPoint u xy' model view
        _ -> return ()

endElasticPoint :: Point -> ModelAndViewUpdate (IO ())
endElasticPoint xy' modelRef view = do
    ec <- readIORef $ elasticContext view
    (endElasticPoint' ec
     <@> clearElasticContext
     >&> clearDragHandlers) modelRef view
    where
        endElasticPoint' Nothing = idModel
        endElasticPoint' (Just (ElasticContext model' (ElasticPoint u))) =
            moveXY u xy'


startElasticPath :: ModelAction () -- make temporary model
                    -> Direction
                    -> LineString -- initial bends
                    -> Point -- mouse position
                    -> ElasticPathContext
                    -> ViewAction
startElasticPath modelF dir bends' xy' ctx model view = do
    writeIORef' (elasticContext view)
        . Just
        . ElasticContext (S.execState modelF model)
        $ ElasticPath dir bends' xy' ctx

startElasticPathFromNode :: Node -> Point -> ViewAction
startElasticPathFromNode u xy' =
    startElasticPath idModel Forward [] xy' (FromNode u)

startElasticPathFromEdgeEnd :: EdgeEnd -> Point -> ViewAction
startElasticPathFromEdgeEnd movingEnd xy' model view =
    startElasticPath (deleteEdge e) fixedDir bends xy'
        (FromEdgeEnd fixedNode fixedEnd e movingEnd movingNode)
        model view
    where bends = edgeBends model e
          fixedDir = endDirection model fixedEnd
          fixedNode = endNode model fixedEnd
          fixedEnd = endOther model movingEnd
          e = endEdge model movingEnd
          movingNode = endNode model movingEnd

startElasticPathFromEdge ::
    Point
    -> (Edge -> Int -> EdgeDiamond -> ElasticPathContext)
    -> Edge -> Int -> Point -> ViewAction
startElasticPathFromEdge exy fromEdge e i xy' model view =
    startElasticPath (S.put model') Forward [] xy' ctx model view
    where (d, model') = S.runState (splitEdge e exy diamondId' undefined undefined) model
          diamondId' d _ _ = return d
          ctx = fromEdge e i d

startElasticPathFromEdgeBend :: Edge -> Int -> Point -> ViewAction
startElasticPathFromEdgeBend e i xy' model view =
    startElasticPathFromEdge dxy FromEdgeBend e i xy' model view
    where dxy = edgeBends model e !! (i-1)

startElasticPathFromEdgeSegment :: Edge -> Int -> Point -> ViewAction
startElasticPathFromEdgeSegment e i xy' model view =
    startElasticPathFromEdge dxy FromEdgeSegment e i xy' model view
    where dxy = projection xy' $ edgeGeometry model e

startElasticPathFromEdgeDiamond :: EdgeDiamond -> Point -> ViewAction
startElasticPathFromEdgeDiamond d xy' =
    startElasticPath idModel Forward [] xy' (FromEdgeDiamond d)

startElasticPathFromBranchTip ::
    Direction
    -> (EdgeDiamond -> EdgeBranch -> Node -> ElasticPathContext)
    -> EdgeBranch -> Point -> ViewAction
startElasticPathFromBranchTip dir fromBranchTip b xy' model view =
    startElasticPath (detachBranch b) dir bends xy' ctx model view
    where bends = branchBends model b
          ctx = fromBranchTip d b w
          d = branchDiamond model b
          w = branchNode model b

startElasticPathFromBranchStart :: EdgeBranch -> Point -> ViewAction
startElasticPathFromBranchStart =
    startElasticPathFromBranchTip Reverse FromBranchStart

startElasticPathFromBranchEnd :: EdgeBranch -> Point -> ViewAction
startElasticPathFromBranchEnd =
    startElasticPathFromBranchTip Forward FromBranchEnd

modifyElasticPathBends :: (LineString -> LineString) -> Point -> ViewAction
modifyElasticPathBends f xy' model view = do
    ec <- readIORef $ elasticContext view
    modify' ec
    where modify' Nothing = return ()
          modify' (Just (ElasticContext model'
                         (ElasticPath dir bends _ ctx))) =
              writeIORef' (elasticContext view)
                  . Just
                  . ElasticContext model'
                  $ ElasticPath dir bends' xy' ctx
                  where bends' = dir ^.^ f $ bends

moveElasticPath :: Point -> ViewAction
moveElasticPath xy' = modifyElasticPathBends id xy'


releaseElasticPath :: Point -> ModelAndViewUpdate (IO ())
releaseElasticPath xy' modelRef view = do
    ec <- readIORef $ elasticContext view
    case ec of
        Nothing -> (idModel <@> endDrag) modelRef view
        Just (ElasticContext m ep) -> do
            model <- readIORef modelRef
            (release' m ep (hitTest model xy')) modelRef view
    where
        endDrag = clearElasticContext >&> clearDragHandlers

        release' :: Model -> ElasticElement -> HitTest -> ModelAndViewUpdate (IO ())

        release' _ (ElasticPath dir bs _ (FromNode u)) (OnNode v')
            | u === v' && null bs = idModel <@> endDrag >&> editNode u
            | otherwise = addEdge dir u v' bs <@> endDrag

        release' _ (ElasticPath _ bs _ (FromNode u)) (OnEdgeDiamond d) =
            addBranch d u (reverse bs) <@> endDrag
        release' _ (ElasticPath _ bs _ (FromEdgeDiamond d)) (OnNode w) =
            addBranch d w bs <@> endDrag
        release' _ (ElasticPath _ bs _ (FromNode u)) (OnEdgeSegment e _) =
            splitEdge e xy' addBranch u (reverse bs) <@> endDrag
        release' m (ElasticPath _ bs _ (FromEdgeSegment e _ d)) (OnNode w) =
            splitEdge e dxy addBranch w bs <@> endDrag
            where dxy = diamondXY m d
        release' _ (ElasticPath _ bs _ (FromNode u)) (OnEdgeBend e i) =
            splitAtBend e i addBranch u (reverse bs) <@> endDrag
        release' _ (ElasticPath _ bs _ (FromEdgeBend e i _)) (OnNode w) =
            splitAtBend e i addBranch w bs <@> endDrag

        release' _ (ElasticPath _ bs _ (FromEdgeEnd _ _ _ me _)) (OnNode m') =
            reconnectEdgeEnd me m' bs <@> endDrag
        release' _ (ElasticPath _ bs _ (FromBranchEnd _ we _)) (OnNode w') =
            reconnectBranchNode we w' bs <@> endDrag

        release' _ (ElasticPath dir bs _ (FromEdgeEnd _ fe _ _ _))
                 (OnEdgeDiamond e') =
            makeBranchFromEdge e' fe (dir ^$^ reverse bs) <@> endDrag
        release' _ (ElasticPath dir bs _ (FromEdgeEnd _ fe _ _ _))
                 (OnEdgeSegment e' _) =
            splitEdge e' xy' makeBranchFromEdge fe (dir ^$^ reverse bs)
            <@> endDrag
        release' _ (ElasticPath dir bs _ (FromEdgeEnd _ fe _ _ _))
                 (OnEdgeBend e' i) =
            splitAtBend e' i makeBranchFromEdge fe (dir ^$^ reverse bs)
            <@> endDrag

        release' _ (ElasticPath _ bs _ (FromBranchStart _ we _)) (OnNode u') =
            makeEdgeFromBranch we u' bs <@> endDrag

        release' _ (ElasticPath _ bs _ (FromBranchStart _ we _))
                 (OnEdgeDiamond e') =
            reconnectBranchDiamond e' we bs <@> endDrag
        release' _ (ElasticPath _ bs _ (FromBranchStart _ we _))
                 (OnEdgeSegment e' _) =
            splitEdge e' xy' reconnectBranchDiamond we bs <@> endDrag
        release' _ (ElasticPath _ bs _ (FromBranchStart _ we _))
                 (OnEdgeBend e' i) =
            splitAtBend e' i reconnectBranchDiamond we bs <@> endDrag

        release' _ (ElasticPath _ _ _ _) _ =
            idModel <@> modifyElasticPathBends (++ [xy']) xy'

        release' _ _ _ = idModel <@> endDrag


editNode :: Node -> ViewAction
editNode n model view = do
    writeIORef' (editContext view) $ Just $ EditNode n
    let editor = textview view
        (x, y) = nodeXY model n
        (w, h) = nodeSize model n
        [ww, hh] = map ceiling [w, h]
        [left, top] = map floor [x - w/2, y - h/2]
    buffer <- textViewGetBuffer editor
    textBufferSetText buffer $ nodeText model n
    widgetSetSizeRequest editor ww hh
    layoutMove (layout view) editor left top
    widgetShow editor
    widgetGrabFocus editor
    writeIORef' (layoutButtonPressHandler view) pressedWhileEditing

endEditing :: ViewAction
endEditing model view = do
    let editor = textview view
    widgetHide editor
    writeIORef' (layoutButtonPressHandler view) pressed
    refreshView model view


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
                "edit_cut_item", "edit_copy_item",
                "edit_paste_item", "edit_delete_item",
                "help_about_item", "node_delete_item", "edge_delete_item"]

    layout <- builderGetObject builder castToLayout "layout"
    textview <- builderGetObject builder castToTextView "textview"

    statusBar <- builderGetObject builder castToStatusbar "status_bar"

    layoutButtonPressHandler <- newIORef pressed
    layoutMotionNotifyHandler <- newIORef $ const idView
    layoutButtonReleaseHandler <- newIORef $ const $ idModel <@> idView

    context <- newIORef NoContext
    elasticContext <- newIORef Nothing
    editContext <- newIORef Nothing

    let view = View builder window
               (Menu fileNewItem fileOpenItem fileSaveItem
                fileSaveAsItem fileQuitItem
                editCutItem editCopyItem editPasteItem
                editDeleteItem helpAboutItem
                nodeDeleteItem edgeDeleteItem)
               nodePopupMenu edgePopupMenu layout textview statusBar
               layoutButtonPressHandler
               layoutMotionNotifyHandler
               layoutButtonReleaseHandler
               context elasticContext editContext

    return view

renderView :: DrawWindow -> ViewAction
renderView drawWindow model view = renderWithDrawable drawWindow $ do
    setSourceRGB 0 0 0
    setLineWidth 1
    ec <- liftIO $ readIORef (elasticContext view)
    let model' = maybe model elasticModel ec
    mapM_ (renderNode model') $ modelNodes model'
    mapM_ (renderEdge model') $ modelEdges model'
    mapM_ (renderDiamond model') $ modelEdgeDiamonds model'
    mapM_ (renderBranch model') $ modelEdgeBranches model'
    case ec of
        (Just (ElasticContext _ ep@(ElasticPath {}))) ->
            renderElasticPath model' ep
        _ -> return ()

elasticModel :: ElasticContext -> Model
elasticModel (ElasticContext m _) = m

renderText :: String -> IO (PangoLayout, Size)
renderText text = do
    context <- cairoCreateContext Nothing
    font <- fontDescriptionFromString "Liberation Sans 10"
    contextSetFontDescription context font
    layout <- layoutText context text
    (Rectangle inkx _ inkw _, Rectangle _ _ _ logh) <-
        layoutGetPixelExtents layout
    let width = fromIntegral $ abs inkx + inkw
        height = fromIntegral logh
    layoutContextChanged layout
    return (layout, (width, height))

measureText :: String -> IO Size
measureText text = do
    (_, (w, h)) <- renderText text
    return (w + 6, h + 6)

renderNode :: Model -> Node -> Render ()
renderNode model' n = do
    let (x, y) = nodeXY model' n
        (w, h) = nodeSize model' n
        text = nodeText model' n
    renderClosed . snapToPixels $ nodeBoundary model' n
    (layout, (width, height)) <- liftIO $ renderText text
    translate (x - width / 2) (y - height / 2)
    showLayout layout
    identityMatrix

renderEdge :: Model -> Edge -> Render ()
renderEdge model' e = renderArrowLineString . snapToPixels $ edgeGeometry model' e

renderDiamond :: Model -> EdgeDiamond -> Render ()
renderDiamond model' d =
    renderLineString
    . snapToPixels
    $ moveGeometry (diamondXY model' d)
      [(nodeRadius, 0), (0, (-nodeRadius)),
       ((-nodeRadius), 0), (0, nodeRadius),
       (nodeRadius, 0)]

renderBranch :: Model -> EdgeBranch -> Render ()
renderBranch model' b = renderLineString . snapToPixels $ branchGeometry model' b

renderElasticPath :: Model -> ElasticElement -> Render ()
renderElasticPath m' (ElasticPath dir bs xy ctx) = case ctx of
    FromNode n
        | null bs && hitTest m' xy == OnNode n -> return ()
        | otherwise -> arrow $   dir ^.^ snapToNode m'  n . (++ [xy]) $ bs
    FromEdgeEnd fn _ _ _ _ -> arrow $   dir ^.^ snapToNode m' fn . (++ [xy]) $ bs
    FromBranchStart _ _ n -> line $ Reverse ^.^ snapToNode m'  n . (++ [xy]) $ bs
    FromEdgeBend    _ _ d -> line $ snapToDiamond m' d $ bs ++ [xy]
    FromEdgeSegment _ _ d -> line $ snapToDiamond m' d $ bs ++ [xy]
    FromEdgeDiamond     d -> line $ snapToDiamond m' d $ bs ++ [xy]
    FromBranchEnd   d b n -> line $ snapToDiamond m' d $ bs ++ [xy]
    where arrow = renderArrowLineString . snapToPixels
          line = renderLineString . snapToPixels

snapToPixels :: LineString -> LineString
snapToPixels = map snapPoint
    where snapPoint = snapCoord *** snapCoord
          snapCoord = (+0.5) . fromIntegral . floor

renderClosed :: LineString -> Render ()
renderClosed [] = return ()
renderClosed g@((x0,y0):xys) = do
    moveTo x0 y0
    mapM_ (uncurry lineTo) xys
    closePath
    stroke

renderLineString :: LineString -> Render ()
renderLineString [] = return ()
renderLineString g@((x0,y0):xys) = do
    moveTo x0 y0
    mapM_ (uncurry lineTo) xys
    stroke

renderArrowLineString :: LineString -> Render ()
renderArrowLineString [] = return ()
renderArrowLineString g@((x0,y0):xys) = do
    renderLineString g
    renderArrowhead 10 (pi/6) xyN1 xyN
    stroke
    where xyN : xyN1 : _ = reverse g

renderArrowhead :: Double -> Double -> Point -> Point -> Render ()
renderArrowhead arrowSize psi (x,y) (x',y') = do
    let phi = atan2 (y' - y) (x' - x)
    moveTo (x' - arrowSize * cos (phi - psi))
           (y' - arrowSize * sin (phi - psi))
    lineTo x' y'
    lineTo (x' - arrowSize * cos (phi + psi))
           (y' - arrowSize * sin (phi + psi))
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

popupEdgeMenu :: EdgeElement edge => edge
                 -> Maybe (MouseButton, TimeStamp)
                 -> ViewAction
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
                (messageDialogNew parent [DialogModal] MessageWarning
                 ButtonsOkCancel text)
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
        handler click button (Just (button, timestamp))
                xy (hitTest model xy) m v
    return True

pressed :: Click -> MouseButton -> Maybe (MouseButton, TimeStamp)
           -> (Double, Double) -> HitTest
           -> ModelAndViewUpdate (IO ())
pressed SingleClick LeftButton _ xy Nowhere = addNewNode xy
pressed SingleClick LeftButton _ xy (OnNode n) =
    dragPath $ startElasticPathFromNode n xy
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
pressed SingleClick RightButton bt _ (OnEdgeBend e _) =
    idModel <@> popupEdgeMenu e bt
pressed SingleClick RightButton bt _ (OnEdgeSegment e _) =
    idModel <@> popupEdgeMenu e bt
pressed SingleClick RightButton bt _ (OnEdgeEnd end) = \mref view -> do
    m <- readIORef mref
    (idModel <@> popupEdgeMenu (endEdge m end) bt) mref view
pressed SingleClick RightButton bt _ (OnEdgeDiamond d) =
    idModel <@> popupEdgeMenu d bt
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
pressedWhileEditing _ _ _ _ _ mref view = do
    Just (EditNode n) <- readIORef $ editContext view
    let editor = textview view
    buffer <- textViewGetBuffer editor
    text <- get buffer textBufferText
    model <- readIORef mref
    size <- measureText text
    ((resize n size >> setText n text) <@> endEditing) mref view

layoutMotionNotify :: ModelAndViewUpdate (EventM EMotion Bool)
layoutMotionNotify m v = do
    xy <- eventCoordinates
    liftIO $ do
        handler <- readIORef $ layoutMotionNotifyHandler v
        (idModel
         <@> handler xy
         >&> refreshView
         >&> showInStatus (`hitTest` xy)) m v
    return True

layoutButtonReleased :: ModelAndViewUpdate (EventM EButton Bool)
layoutButtonReleased m v = do
    xy <- eventCoordinates
    liftIO $ do
        handler <- readIORef $ layoutButtonReleaseHandler v
        (handler xy >>> idModel <@> refreshView) m v
    return True

addNewNode :: Point -> ModelAndViewUpdate (IO ())
addNewNode xy = addNode xy (20, 20) "" <@> refreshView
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
dragPath va = idModel <@> va
                      >&> refreshView
                      >&> setDragHandlers moveElasticPath releaseElasticPath

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

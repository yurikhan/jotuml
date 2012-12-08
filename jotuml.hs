import Control.Exception.Base
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Data.IORef
import Debug.Trace
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Windows.MessageDialog

-- workaround
modifyIORef'' :: IORef a -> (a -> a) -> IO ()
modifyIORef'' ref f = do
    x <- readIORef ref
    let x' = f x
    x' `seq` writeIORef ref x'


data UI = UI { builder :: Builder,
               window :: Window,
               layout :: Layout,
               aboutBox :: Dialog,
               fileNewItem :: MenuItem,
               fileOpenItem :: MenuItem,
               fileSaveItem :: MenuItem,
               fileSaveAsItem :: MenuItem,
               fileQuitItem :: MenuItem,
               editCutItem :: MenuItem,
               editCopyItem :: MenuItem,
               editPasteItem :: MenuItem,
               editDeleteItem :: MenuItem,
               helpAboutItem :: MenuItem
             }

type ModelRef = IORef Model

data Model = Model { modelVertices :: [Vertex],
                     -- TODO: modelEdges :: [Edge],
                     modelDiagram :: Diagram }

newModel = Model [] newDiagram

data Vertex = Vertex { vertexKind :: VertexKind,
                       vertexCompartments :: [Compartment] }

data VertexKind = Class -- TODO: | Interface | Package | Node | Component | Actor | UseCase

newVertex = Vertex Class [Compartment "Hello"]

data Compartment = Compartment { compartmentText :: String }

measureCompartment :: PangoLayout -> (Double, Double, [Double]) -> Compartment -> Render (Double, Double, [Double])
measureCompartment layout (oldWidth, oldHeight, oldRules) compartment = do
    liftIO $ layoutSetText layout $ compartmentText compartment
    (Rectangle inkx _ inkw _, Rectangle _ _ _ logh) <- liftIO $ layoutGetPixelExtents layout
    let width = 4 + (abs inkx) + inkw
        height = 6 + logh
        newHeight = oldHeight + fromIntegral height
    return (max oldWidth $ fromIntegral width, newHeight, newHeight : oldRules)

renderCompartment :: PangoLayout -> Double -> Double -> (Double, Compartment) -> Render ()
renderCompartment layout left top (y, compartment) = do
    liftIO $ layoutSetText layout $ compartmentText compartment
    liftIO $ layoutContextChanged layout
    translate (left + 2) (top + y + 3)
    showLayout layout
    identityMatrix


data Diagram = Diagram { diagramViews :: [View] }

newDiagram = Diagram []

renderDiagram :: Diagram -> Render ()
renderDiagram diagram = mapM_ renderView $ diagramViews diagram

data View = VertexView { viewVertex :: Vertex,
                         viewCx :: Double,
                         viewCy :: Double,
                         viewIsNew :: Bool }

renderView :: View -> Render ()
renderView (VertexView v cx cy _) = do
    setSourceRGB 0 0 0
    setAntialias AntialiasSubpixel
    context <- liftIO $ cairoCreateContext Nothing
    liftIO $ fontDescriptionFromString "sans 10" >>= contextSetFontDescription context
    layout <- liftIO $ layoutEmpty context
    liftIO $ layoutSetIndent layout (-16384)
    (width, height, rules) <- foldM (measureCompartment layout) (0, 0, []) $ vertexCompartments v
    let left = cx - width / 2
        top = cy - height / 2
        right = cx + width / 2
    setLineWidth 1
    rectangle left top width height
    mapM (drawRule left right) (init rules)
    stroke
    mapM (renderCompartment layout left top) (zip (0:reverse rules) $ vertexCompartments v)
    return ()
    where drawRule leftx rightx y = do
              moveTo leftx y
              lineTo rightx y

addNewClassView :: Double -> Double -> Model -> Model
addNewClassView x y oldModel = Model (newVertex:oldVertices) newDiagram where
    oldVertices = modelVertices oldModel
    newDiagram = Diagram $ newView:oldViews
    newView = VertexView newVertex x y True
    oldViews = diagramViews $ modelDiagram oldModel


layoutButtonPressed :: UI -> ModelRef -> EventM EButton Bool
layoutButtonPressed ui modelRef = tryEvent $ do
    (x, y) <- eventCoordinates
    liftIO $ modifyIORef'' modelRef $ addNewClassView x y
    liftIO $ widgetQueueDraw $ layout ui
    return ()

layoutButtonReleased :: UI -> ModelRef -> EventM EButton Bool
layoutButtonReleased ui modelRef = tryEvent $ do
    return ()

layoutExposed :: UI -> ModelRef -> EventM EExpose Bool
layoutExposed ui modelRef = tryEvent $ do
    drawWindow <- eventWindow
    model <- liftIO $ readIORef modelRef
    liftIO $ renderWithDrawable drawWindow $ renderDiagram $ modelDiagram model
    return ()


loadUI :: IO ()
loadUI = do
    builder <- builderNew
    builderAddFromFile builder "jotuml.ui"

    window <- builderGetObject builder castToWindow "main_window"
    on window objectDestroy mainQuit
    on window deleteEvent $ do
        liftIO $ confirm (Just window) quitConfirmation (return False) (return True)

    layout <- builderGetObject builder castToLayout "layout"

    aboutDialog <- builderGetObject builder castToDialog "about_dialog"

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

    let ui = UI builder window layout aboutDialog fileNewItem fileOpenItem fileSaveItem
             fileSaveAsItem fileQuitItem editCutItem editCopyItem editPasteItem editDeleteItem
             helpAboutItem

    modelRef <- newIORef newModel

    on fileQuitItem menuItemActivate $ confirm (Just window) quitConfirmation mainQuit $ return ()
    on helpAboutItem menuItemActivate $ dialogRun aboutDialog `finally` widgetHide aboutDialog >> return ()

    on layout buttonPressEvent $ layoutButtonPressed ui modelRef
    on layout buttonReleaseEvent $ layoutButtonReleased ui modelRef
    on layout exposeEvent $ layoutExposed ui modelRef

    return ()

confirm :: Maybe Window -> String -> IO a -> IO a -> IO a
confirm parent text action inaction = do
    response <- bracket
                (messageDialogNew parent [DialogModal] MessageWarning ButtonsOkCancel text)
                widgetDestroy
                dialogRun
    if (response == ResponseOk) then action else inaction

quitConfirmation = "Do you really want to quit?"

main :: IO ()
main = do
    initGUI
    loadUI
    mainGUI

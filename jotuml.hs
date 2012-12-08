import Control.Exception.Base
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Debug.Trace
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Windows.MessageDialog

data UI = UI { builder :: Builder,
               window :: Window,
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

loadUI :: IO UI
loadUI = do
    builder <- builderNew
    builderAddFromFile builder "jotuml.ui"

    window <- builderGetObject builder castToWindow "main_window"
    on window objectDestroy mainQuit
    on window deleteEvent $ do
        liftIO $ confirm (Just window) quitConfirmation (return False) (return True)

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

    on fileQuitItem menuItemActivate $ confirm (Just window) quitConfirmation mainQuit $ return ()
    on helpAboutItem menuItemActivate $ dialogRun aboutDialog `finally` widgetHide aboutDialog >> return ()

    return $ UI builder window aboutDialog fileNewItem fileOpenItem fileSaveItem
        fileSaveAsItem fileQuitItem editCutItem editCopyItem editPasteItem editDeleteItem
        helpAboutItem

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
    ui <- loadUI
    mainGUI

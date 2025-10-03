module Common.Utils.ShortcutUtils exposing (primaryShortcut)

import Shortcut exposing (Shortcut)


primaryShortcut : Bool -> Shortcut.Key -> msg -> Shortcut msg
primaryShortcut isMac key =
    if isMac then
        Shortcut.metaShortcut key

    else
        Shortcut.ctrlShortcut key

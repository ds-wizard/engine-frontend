module Common.Utils.ShortcutUtils exposing (primaryShortcut, submitShortcut)

import Shortcut exposing (Shortcut)


primaryShortcut : Bool -> Shortcut.Key -> msg -> Shortcut msg
primaryShortcut isMac key =
    if isMac then
        Shortcut.metaShortcut key

    else
        Shortcut.ctrlShortcut key


submitShortcut : Bool -> msg -> Shortcut msg
submitShortcut isMac =
    primaryShortcut isMac Shortcut.Enter

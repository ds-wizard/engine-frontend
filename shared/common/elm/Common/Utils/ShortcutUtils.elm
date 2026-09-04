module Common.Utils.ShortcutUtils exposing (plainShortcut, primaryShortcut, submitShortcut)

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


{-| Shortcut for a key pressed without the primary modifier (ctrl or cmd).

Useful when the same key is also used with the primary modifier somewhere else so that
pressing the combination does not trigger both shortcuts at once.

-}
plainShortcut : Shortcut.Key -> msg -> Shortcut msg
plainShortcut key msg =
    { msg = msg
    , keyCombination =
        { baseKey = key
        , alt = Nothing
        , shift = Nothing
        , ctrl = Just False
        , meta = Just False
        }
    }

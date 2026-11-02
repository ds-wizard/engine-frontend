port module Common.Ports.Window exposing
    ( alert
    , clearUnloadMessage
    , historyBack
    , historyBackCallback
    , refresh
    , refreshAfter
    , setTitle
    , setUnloadMessage
    )


port alert : String -> Cmd msg


port refresh : () -> Cmd msg


{-| Reloads the application after the given number of milliseconds.
-}
port refreshAfter : Int -> Cmd msg


port historyBack : String -> Cmd msg


port historyBackCallback : (String -> msg) -> Sub msg


{-| The document's title at once. The view sets it too, but only when it is
drawn, and a tab in the background is not drawn until it is shown.
-}
port setTitle : String -> Cmd msg


port setUnloadMessage : String -> Cmd msg


port clearUnloadMessage : () -> Cmd msg

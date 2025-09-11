port module Common.Ports.Window exposing
    ( alert
    , clearUnloadMessage
    , historyBack
    , historyBackCallback
    , refresh
    , setUnloadMessage
    )


port alert : String -> Cmd msg


port refresh : () -> Cmd msg


port historyBack : String -> Cmd msg


port historyBackCallback : (String -> msg) -> Sub msg


port setUnloadMessage : String -> Cmd msg


port clearUnloadMessage : () -> Cmd msg

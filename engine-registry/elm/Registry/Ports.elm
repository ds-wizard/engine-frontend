port module Registry.Ports exposing
    ( clearSession
    , saveSession
    )

import Json.Encode as E


port saveSession : E.Value -> Cmd msg


port clearSession : () -> Cmd msg

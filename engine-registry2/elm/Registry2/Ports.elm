port module Registry2.Ports exposing
    ( clearSession
    , saveSession
    )

import Json.Encode as E


port saveSession : E.Value -> Cmd msg


port clearSession : () -> Cmd msg

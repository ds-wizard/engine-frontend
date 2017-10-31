port module Ports exposing (clearSession, onSessionChange, storeSession)

import Json.Encode exposing (Value)


port storeSession : Maybe String -> Cmd msg


port clearSession : () -> Cmd msg


port onSessionChange : (Value -> msg) -> Sub msg

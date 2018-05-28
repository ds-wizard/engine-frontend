port module Ports exposing (clearSession, onSessionChange, storeSession)

import Auth.Models exposing (Session)
import Json.Encode exposing (Value)


port storeSession : Maybe Session -> Cmd msg


port clearSession : () -> Cmd msg


port onSessionChange : (Value -> msg) -> Sub msg

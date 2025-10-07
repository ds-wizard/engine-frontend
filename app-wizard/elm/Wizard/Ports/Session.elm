port module Wizard.Ports.Session exposing
    ( clearSession
    , clearSessionAndReload
    , storeSession
    )

import Json.Encode as E


port storeSession : E.Value -> Cmd msg


port clearSession : () -> Cmd msg


port clearSessionAndReload : () -> Cmd msg

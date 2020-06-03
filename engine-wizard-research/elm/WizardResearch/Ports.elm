port module WizardResearch.Ports exposing
    ( clearSession
    , storeSession
    )

-- Session

import Json.Encode as E


port storeSession : E.Value -> Cmd msg


port clearSession : () -> Cmd msg

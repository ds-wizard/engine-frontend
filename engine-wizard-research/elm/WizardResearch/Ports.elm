port module WizardResearch.Ports exposing
    ( clearSession
    , replaceUrl
    , storeSession
    )

-- Session

import Json.Encode as E


port storeSession : E.Value -> Cmd msg


port clearSession : () -> Cmd msg


port replaceUrl : String -> Cmd msg

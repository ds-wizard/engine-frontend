port module Registry.Ports exposing (saveCredentials)

import Json.Encode as E


port saveCredentials : E.Value -> Cmd msg

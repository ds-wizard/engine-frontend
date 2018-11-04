module Public.SignupConfirmation.Requests exposing (activateBody, putUserActivation)

import Http
import Json.Encode as Encode exposing (Value)
import Requests exposing (apiUrl)


putUserActivation : String -> String -> Http.Request String
putUserActivation userId hash =
    Http.request
        { method = "PUT"
        , headers = []
        , url = apiUrl "/users" ++ "/" ++ userId ++ "/state?hash=" ++ hash
        , body = Http.jsonBody activateBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }


activateBody : Value
activateBody =
    Encode.object [ ( "active", Encode.bool True ) ]

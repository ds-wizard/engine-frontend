module Public.ForgottenPasswordConfirmation.Requests exposing (putUserPassword)

import Http
import Json.Encode exposing (Value)
import Requests exposing (apiUrl)


putUserPassword : String -> String -> Value -> Http.Request String
putUserPassword userId hash body =
    Http.request
        { method = "PUT"
        , headers = []
        , url = apiUrl "/users/" ++ userId ++ "/password?hash=" ++ hash
        , body = Http.jsonBody body
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }

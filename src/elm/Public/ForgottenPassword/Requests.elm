module Public.ForgottenPassword.Requests exposing (..)

import Http
import Json.Encode exposing (Value)
import Requests exposing (apiUrl)


postPasswordActionKey : Value -> Http.Request String
postPasswordActionKey body =
    Http.request
        { method = "POST"
        , headers = []
        , url = apiUrl "/action-keys"
        , body = Http.jsonBody body
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }

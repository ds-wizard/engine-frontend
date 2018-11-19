module Public.Signup.Requests exposing (postSignup)

import Http
import Json.Encode exposing (Value)
import Requests exposing (apiUrl)


postSignup : Value -> Http.Request String
postSignup body =
    Http.request
        { method = "POST"
        , headers = []
        , url = apiUrl "/users"
        , body = Http.jsonBody body
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }

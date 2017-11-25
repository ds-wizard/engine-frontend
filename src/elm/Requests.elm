module Requests exposing (..)

import Auth.Models exposing (Session)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Jwt


apiUrl : String -> String
apiUrl url =
    apiRoot ++ url


apiRoot : String
apiRoot =
    "http://localhost:3000"


toCmd : (Result Jwt.JwtError a -> a1) -> (a1 -> msg) -> Http.Request a -> Cmd msg
toCmd msg rootMsg req =
    req
        |> Jwt.send msg
        |> Cmd.map rootMsg


get : Session -> String -> Decoder a -> Http.Request a
get session url decoder =
    Jwt.get session.token (apiUrl url) decoder


post : Value -> Session -> String -> Http.Request String
post body =
    emptyResponseRequest "POST" (Http.jsonBody body)


put : Value -> Session -> String -> Http.Request String
put body =
    emptyResponseRequest "PUT" (Http.jsonBody body)


delete : Session -> String -> Http.Request String
delete =
    emptyResponseRequest "DELETE" Http.emptyBody


emptyResponseRequest : String -> Http.Body -> Session -> String -> Http.Request String
emptyResponseRequest method body session url =
    let
        req =
            Jwt.createRequestObject
                method
                session.token
                (apiUrl url)
                body
                (Decode.succeed "")
    in
    { req | expect = Http.expectString } |> Http.request

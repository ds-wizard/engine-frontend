module UserManagement.Requests exposing (..)

import Auth.Models exposing (Session)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Jwt
import Requests exposing (apiUrl)
import UserManagement.Models exposing (User, userDecoder, userListDecoder)


getUsers : Session -> Http.Request (List User)
getUsers session =
    Jwt.get session.token (apiUrl "/users") userListDecoder


getUser : String -> Session -> Http.Request User
getUser uuid session =
    Jwt.get session.token (apiUrl "/users/" ++ uuid) userDecoder


postUser : Session -> Encode.Value -> Http.Request String
postUser session user =
    let
        req =
            Jwt.createRequestObject
                "POST"
                session.token
                (apiUrl "/users")
                (Http.jsonBody user)
                (Decode.succeed "")
    in
    { req | expect = Http.expectString } |> Http.request


putUser : String -> Session -> Encode.Value -> Http.Request String
putUser uuid session user =
    let
        req =
            Jwt.createRequestObject
                "PUT"
                session.token
                (apiUrl "/users/" ++ uuid)
                (Http.jsonBody user)
                (Decode.succeed "")
    in
    { req | expect = Http.expectString } |> Http.request


putUserPassword : String -> Session -> Encode.Value -> Http.Request String
putUserPassword uuid session password =
    let
        req =
            Jwt.createRequestObject
                "PUT"
                session.token
                (apiUrl "/users/" ++ uuid ++ "/password")
                (Http.jsonBody password)
                (Decode.succeed "")
    in
    { req | expect = Http.expectString } |> Http.request


deleteUser : String -> Session -> Http.Request String
deleteUser uuid session =
    let
        req =
            Jwt.createRequestObject
                "DELETE"
                session.token
                (apiUrl "/users/" ++ uuid)
                Http.emptyBody
                (Decode.succeed "")
    in
    { req | expect = Http.expectString } |> Http.request

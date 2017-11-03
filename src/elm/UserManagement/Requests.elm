module UserManagement.Requests exposing (..)

import Auth.Models exposing (Session)
import Http
import Json.Decode as Decode
import Jwt
import Requests exposing (apiUrl)
import UserManagement.Models exposing (User, userDecoder, userListDecoder)


getUsers : Session -> Http.Request (List User)
getUsers session =
    Jwt.get session.token (apiUrl "/users") userListDecoder


getUser : String -> Session -> Http.Request User
getUser uuid session =
    Jwt.get session.token (apiUrl "/users/" ++ uuid) userDecoder


deleteUser : String -> Session -> Http.Request String
deleteUser uuid session =
    let
        req =
            Jwt.createRequestObject "DELETE" session.token (apiUrl "/users/" ++ uuid) Http.emptyBody (Decode.succeed "")
    in
    { req | expect = Http.expectString } |> Http.request

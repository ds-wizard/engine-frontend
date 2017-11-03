module UserManagement.Requests exposing (..)

import Auth.Models exposing (Session)
import Http
import Jwt
import Requests exposing (apiUrl)
import UserManagement.Models exposing (User, userDecoder, userListDecoder)


getUsers : Session -> Http.Request (List User)
getUsers session =
    Jwt.get session.token (apiUrl "/users") userListDecoder

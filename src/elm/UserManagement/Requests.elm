module UserManagement.Requests exposing (..)

import Auth.Models exposing (Session)
import Http
import Json.Encode exposing (Value)
import Requests
import UserManagement.Common.Models exposing (User, userDecoder, userListDecoder)


getUsers : Session -> Http.Request (List User)
getUsers session =
    Requests.get session "/users" userListDecoder


getUser : String -> Session -> Http.Request User
getUser uuid session =
    Requests.get session ("/users/" ++ uuid) userDecoder


postUser : Session -> Value -> Http.Request String
postUser session user =
    Requests.post user session "/users"


putUser : String -> Session -> Value -> Http.Request String
putUser uuid session user =
    Requests.put user session ("/users/" ++ uuid)


putUserPassword : String -> Session -> Value -> Http.Request String
putUserPassword uuid session password =
    Requests.put password session ("/users/" ++ uuid ++ "/password")


deleteUser : String -> Session -> Http.Request String
deleteUser uuid session =
    Requests.delete session ("/users/" ++ uuid)

module Auth.Requests exposing (..)

import Auth.Models exposing (Session)
import Http
import Requests exposing (apiUrl)
import UserManagement.Common.Models exposing (User, userDecoder)


getCurrentUser : Session -> Http.Request User
getCurrentUser session =
    Requests.get session "/users/current" userDecoder

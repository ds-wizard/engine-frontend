module Auth.Requests exposing (getCurrentUser)

import Auth.Models exposing (Session)
import Http
import Requests exposing (apiUrl)
import Users.Common.Models exposing (User, userDecoder)


getCurrentUser : Session -> Http.Request User
getCurrentUser session =
    Requests.get session "/users/current" userDecoder

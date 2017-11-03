module UserManagement.Delete.Models exposing (..)

import UserManagement.Models exposing (User)


type alias Model =
    { user : Maybe User
    , loadingUser : Bool
    , deletingUser : Bool
    , error : String
    }


initialModel : Model
initialModel =
    { user = Nothing
    , loadingUser = True
    , deletingUser = False
    , error = ""
    }

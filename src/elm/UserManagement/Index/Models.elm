module UserManagement.Index.Models exposing (..)

import UserManagement.Models exposing (User)


type alias Model =
    { users : List User
    , loading : Bool
    , error : String
    }


initialModel : Model
initialModel =
    { users = []
    , loading = True
    , error = ""
    }

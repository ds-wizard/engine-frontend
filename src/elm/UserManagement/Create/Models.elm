module UserManagement.Create.Models exposing (..)

import Form exposing (Form)
import UserManagement.Models exposing (UserCreateForm, initUserCreateForm)


type alias Model =
    { form : Form () UserCreateForm
    , savingUser : Bool
    , error : String
    }


initialModel : Model
initialModel =
    { form = initUserCreateForm
    , savingUser = False
    , error = ""
    }

module UserManagement.Edit.Models exposing (..)

import Form exposing (Form)
import UserManagement.Models exposing (..)


type alias Model =
    { uuid : String
    , loading : Bool
    , loadingError : String
    , editForm : Form () UserEditForm
    , editSaving : Bool
    , editError : String
    , passwordForm : Form UserPasswordFormError UserPasswordForm
    , passwordSaving : Bool
    , passwordError : String
    }


initialModel : Model
initialModel =
    { uuid = ""
    , loading = True
    , loadingError = ""
    , editForm = initEmptyUserEditForm
    , editSaving = False
    , editError = ""
    , passwordForm = initUserPasswordForm
    , passwordSaving = False
    , passwordError = ""
    }

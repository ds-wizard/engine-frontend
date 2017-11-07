module UserManagement.Edit.Models exposing (..)

import Form exposing (Form)
import UserManagement.Models exposing (..)
import Utils exposing (FormResult(..))


type alias Model =
    { uuid : String
    , loading : Bool
    , loadingError : String
    , editForm : Form () UserEditForm
    , editSaving : Bool
    , editResult : FormResult
    , passwordForm : Form UserPasswordFormError UserPasswordForm
    , passwordSaving : Bool
    , passwordResult : FormResult
    }


initialModel : String -> Model
initialModel uuid =
    { uuid = uuid
    , loading = True
    , loadingError = ""
    , editForm = initEmptyUserEditForm
    , editSaving = False
    , editResult = None
    , passwordForm = initUserPasswordForm
    , passwordSaving = False
    , passwordResult = None
    }

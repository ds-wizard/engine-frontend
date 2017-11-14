module UserManagement.Edit.Models exposing (..)

import Common.Types exposing (ActionResult(..))
import Form exposing (Form)
import UserManagement.Models exposing (..)


type alias Model =
    { uuid : String
    , user : ActionResult User
    , savingUser : ActionResult String
    , savingPassword : ActionResult String
    , userForm : Form () UserEditForm
    , passwordForm : Form UserPasswordFormError UserPasswordForm
    }


initialModel : String -> Model
initialModel uuid =
    { uuid = uuid
    , user = Loading
    , savingUser = Unset
    , savingPassword = Unset
    , userForm = initEmptyUserEditForm
    , passwordForm = initUserPasswordForm
    }

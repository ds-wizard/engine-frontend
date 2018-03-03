module UserManagement.Edit.Models exposing (..)

import Common.Form.Validate exposing (CustomFormError)
import Common.Types exposing (ActionResult(..))
import Form exposing (Form)
import UserManagement.Models exposing (..)


type View
    = Profile
    | Password


type alias Model =
    { uuid : String
    , currentView : View
    , user : ActionResult User
    , savingUser : ActionResult String
    , savingPassword : ActionResult String
    , userForm : Form () UserEditForm
    , passwordForm : Form CustomFormError UserPasswordForm
    }


initialModel : String -> Model
initialModel uuid =
    { uuid = uuid
    , currentView = Profile
    , user = Loading
    , savingUser = Unset
    , savingPassword = Unset
    , userForm = initEmptyUserEditForm
    , passwordForm = initUserPasswordForm
    }

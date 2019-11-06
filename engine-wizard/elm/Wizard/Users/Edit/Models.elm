module Wizard.Users.Edit.Models exposing
    ( Model
    , View(..)
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Users.Common.User exposing (User)
import Wizard.Users.Common.UserEditForm as UserEditForm exposing (UserEditForm)
import Wizard.Users.Common.UserPasswordForm as UserPasswordForm exposing (UserPasswordForm)


type View
    = Profile
    | Password


type alias Model =
    { uuid : String
    , currentView : View
    , user : ActionResult User
    , savingUser : ActionResult String
    , savingPassword : ActionResult String
    , userForm : Form CustomFormError UserEditForm
    , passwordForm : Form CustomFormError UserPasswordForm
    }


initialModel : String -> Model
initialModel uuid =
    { uuid = uuid
    , currentView = Profile
    , user = Loading
    , savingUser = Unset
    , savingPassword = Unset
    , userForm = UserEditForm.initEmpty
    , passwordForm = UserPasswordForm.init
    }

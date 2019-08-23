module Users.Create.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Common.Form exposing (CustomFormError)
import Form exposing (Form)
import Users.Common.UserCreateForm as UserCreateForm exposing (UserCreateForm)


type alias Model =
    { savingUser : ActionResult String
    , form : Form CustomFormError UserCreateForm
    }


initialModel : Model
initialModel =
    { savingUser = Unset
    , form = UserCreateForm.init
    }

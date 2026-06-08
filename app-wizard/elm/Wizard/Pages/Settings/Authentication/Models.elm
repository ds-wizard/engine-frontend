module Wizard.Pages.Settings.Authentication.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult)
import Common.Api.Models.Role exposing (Role)
import Wizard.Pages.Settings.Common.Forms.AuthenticationConfigForm as AuthenticationConfigForm exposing (AuthenticationConfigForm)
import Wizard.Pages.Settings.Generic.Model as GenericModel


type alias Model =
    { genericModel : GenericModel.Model AuthenticationConfigForm
    , roles : ActionResult (List Role)
    }


initialModel : Model
initialModel =
    { genericModel = GenericModel.initialModel AuthenticationConfigForm.initEmpty
    , roles = ActionResult.Loading
    }

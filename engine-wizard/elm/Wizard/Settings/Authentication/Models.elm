module Wizard.Settings.Authentication.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult)
import Shared.Data.EditableConfig.EditableAuthenticationConfig.EditableOpenIDServiceConfig exposing (EditableOpenIDServiceConfig)
import Wizard.Settings.Common.Forms.AuthenticationConfigForm as AuthenticationConfigForm exposing (AuthenticationConfigForm)
import Wizard.Settings.Generic.Model as GenericModel


type alias Model =
    { genericModel : GenericModel.Model AuthenticationConfigForm
    , openIDPrefabs : ActionResult (List EditableOpenIDServiceConfig)
    }


initialModel : Model
initialModel =
    { genericModel = GenericModel.initialModel AuthenticationConfigForm.initEmpty
    , openIDPrefabs = ActionResult.Loading
    }

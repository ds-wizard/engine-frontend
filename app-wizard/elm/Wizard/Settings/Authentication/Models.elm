module Wizard.Settings.Authentication.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult)
import Wizard.Api.Models.EditableConfig.EditableAuthenticationConfig.EditableOpenIDServiceConfig exposing (EditableOpenIDServiceConfig)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Settings.Common.Forms.AuthenticationConfigForm as AuthenticationConfigForm exposing (AuthenticationConfigForm)
import Wizard.Settings.Generic.Model as GenericModel


type alias Model =
    { genericModel : GenericModel.Model AuthenticationConfigForm
    , openIDPrefabs : ActionResult (List EditableOpenIDServiceConfig)
    }


initialModel : AppState -> Model
initialModel appState =
    { genericModel = GenericModel.initialModel (AuthenticationConfigForm.initEmpty appState)
    , openIDPrefabs = ActionResult.Loading
    }

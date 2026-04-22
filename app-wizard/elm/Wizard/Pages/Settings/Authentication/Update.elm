module Wizard.Pages.Settings.Authentication.Update exposing (update)

import Wizard.Api.Models.EditableConfig as EditableConfig
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Settings.Authentication.Models exposing (Model)
import Wizard.Pages.Settings.Common.Forms.AuthenticationConfigForm as AuthenticationConfigForm exposing (AuthenticationConfigForm)
import Wizard.Pages.Settings.Generic.Msgs exposing (Msg)
import Wizard.Pages.Settings.Generic.Update as GenericUpdate


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update =
    GenericUpdate.update updateProps


updateProps : GenericUpdate.UpdateProps AuthenticationConfigForm
updateProps =
    { initForm = AuthenticationConfigForm.init << .authentication
    , formToConfig = EditableConfig.updateAuthentication << AuthenticationConfigForm.toEditableAuthConfig
    , formValidation = AuthenticationConfigForm.validation
    }

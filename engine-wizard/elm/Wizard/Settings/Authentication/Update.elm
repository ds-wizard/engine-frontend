module Wizard.Settings.Authentication.Update exposing (update)

import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Settings.Authentication.Models exposing (Model)
import Wizard.Settings.Common.EditableConfig as EditableConfig
import Wizard.Settings.Common.Forms.AuthenticationConfigForm as AuthenticationConfigForm exposing (AuthenticationConfigForm)
import Wizard.Settings.Generic.Msgs exposing (Msg)
import Wizard.Settings.Generic.Update as GenericUpdate


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update =
    GenericUpdate.update updateProps


updateProps : GenericUpdate.UpdateProps AuthenticationConfigForm
updateProps =
    { initForm = .authentication >> AuthenticationConfigForm.init
    , formToConfig = AuthenticationConfigForm.toEditableAuthConfig >> EditableConfig.updateAuthentication
    , formValidation = AuthenticationConfigForm.validation
    }

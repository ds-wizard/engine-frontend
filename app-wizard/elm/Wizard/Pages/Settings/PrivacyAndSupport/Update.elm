module Wizard.Pages.Settings.PrivacyAndSupport.Update exposing (update)

import Wizard.Api.Models.BootstrapConfig.PrivacyAndSupportConfig as PrivacyAndSupportConfig exposing (PrivacyAndSupportConfig)
import Wizard.Api.Models.EditableConfig as EditableConfig
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Settings.Generic.Msgs exposing (Msg)
import Wizard.Pages.Settings.Generic.Update as GenericUpdate
import Wizard.Pages.Settings.PrivacyAndSupport.Models exposing (Model)


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update =
    GenericUpdate.update updateProps


updateProps : GenericUpdate.UpdateProps PrivacyAndSupportConfig
updateProps =
    { initForm = .privacyAndSupport >> PrivacyAndSupportConfig.initForm
    , formToConfig = EditableConfig.updatePrivacyAndSupport
    , formValidation = PrivacyAndSupportConfig.validation
    }

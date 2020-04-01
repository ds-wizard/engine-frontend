module Wizard.Settings.PrivacyAndSupport.Update exposing (update)

import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Config.PrivacyAndSupportConfig as PrivacyAndSupportConfig exposing (PrivacyAndSupportConfig)
import Wizard.Msgs
import Wizard.Settings.Common.EditableConfig as EditableConfig
import Wizard.Settings.Generic.Msgs exposing (Msg)
import Wizard.Settings.Generic.Update as GenericUpdate
import Wizard.Settings.PrivacyAndSupport.Models exposing (Model)


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update =
    GenericUpdate.update updateProps


updateProps : GenericUpdate.UpdateProps PrivacyAndSupportConfig
updateProps =
    { initForm = .privacyAndSupport >> PrivacyAndSupportConfig.initForm
    , formToConfig = EditableConfig.updatePrivacyAndSupport
    , formValidation = PrivacyAndSupportConfig.validation
    }

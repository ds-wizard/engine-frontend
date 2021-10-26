module Wizard.Settings.LookAndFeel.Update exposing (fetchData, update)

import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig exposing (LookAndFeelConfig)
import Shared.Data.EditableConfig as EditableConfig
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Settings.Generic.Update as GenericUpdate
import Wizard.Settings.LookAndFeel.LogoUploadModal as LogoUploadModal
import Wizard.Settings.LookAndFeel.Models exposing (Model)
import Wizard.Settings.LookAndFeel.Msgs exposing (Msg(..))


fetchData : AppState -> Cmd Msg
fetchData =
    Cmd.map GenericMsg << GenericUpdate.fetchData


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        GenericMsg genericMsg ->
            let
                ( genericModel, cmd ) =
                    GenericUpdate.update updateProps (wrapMsg << GenericMsg) genericMsg appState model.genericModel
            in
            ( { model | genericModel = genericModel }, cmd )

        LogoUploadModalMsg logoUploadModalMsg ->
            let
                ( logoUploadModalModel, cmd ) =
                    LogoUploadModal.update (wrapMsg << LogoUploadModalMsg) (Ports.refresh ()) logoUploadModalMsg appState model.logoUploadModalModel
            in
            ( { model | logoUploadModalModel = logoUploadModalModel }, cmd )


updateProps : GenericUpdate.UpdateProps LookAndFeelConfig
updateProps =
    { initForm = .lookAndFeel >> LookAndFeelConfig.initForm
    , formToConfig = EditableConfig.updateLookAndFeel
    , formValidation = LookAndFeelConfig.validation
    }

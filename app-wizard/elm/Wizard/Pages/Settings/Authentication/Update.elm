module Wizard.Pages.Settings.Authentication.Update exposing (fetchData, update)

import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Wizard.Api.Models.EditableConfig as EditableConfig
import Wizard.Api.Roles as RolesApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Settings.Authentication.Models exposing (Model)
import Wizard.Pages.Settings.Authentication.Msgs exposing (Msg(..))
import Wizard.Pages.Settings.Common.Forms.AuthenticationConfigForm as AuthenticationConfigForm exposing (AuthenticationConfigForm)
import Wizard.Pages.Settings.Generic.Update as GenericUpdate


fetchData : AppState -> Cmd Msg
fetchData appState =
    Cmd.batch
        [ Cmd.map GenericMsg (GenericUpdate.fetchData appState)
        , RolesApi.getRoles appState GetRolesCompleted
        ]


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        GenericMsg genericMsg ->
            let
                ( genericModel, cmd ) =
                    GenericUpdate.update updateProps (wrapMsg << GenericMsg) genericMsg appState model.genericModel
            in
            ( { model | genericModel = genericModel }, cmd )

        GetRolesCompleted result ->
            RequestHelpers.applyResultTransform
                { setResult = \r m -> { m | roles = r }
                , defaultError = gettext "Unable to get roles." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , transform = .items
                , locale = appState.locale
                }


updateProps : GenericUpdate.UpdateProps AuthenticationConfigForm
updateProps =
    { initForm = AuthenticationConfigForm.init << .authentication
    , formToConfig = EditableConfig.updateAuthentication << AuthenticationConfigForm.toEditableAuthConfig
    , formValidation = AuthenticationConfigForm.validation
    }

module Wizard.Pages.Settings.Authentication.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Wizard.Api.Models.EditableConfig as EditableConfig
import Wizard.Api.Prefabs as PrefabsApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Settings.Authentication.Models exposing (Model)
import Wizard.Pages.Settings.Authentication.Msgs exposing (Msg(..))
import Wizard.Pages.Settings.Common.Forms.AuthenticationConfigForm as AuthenticationConfigForm
import Wizard.Pages.Settings.Generic.Update as GenericUpdate


fetchData : AppState -> Cmd Msg
fetchData appState =
    Cmd.batch
        [ Cmd.map GenericMsg <| GenericUpdate.fetchData appState
        , PrefabsApi.getOpenIDPrefabs appState GetOpenIDPrefabsComplete
        ]


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        GenericMsg genericMsg ->
            let
                updateProps =
                    { initForm = AuthenticationConfigForm.init appState << .authentication
                    , formToConfig = AuthenticationConfigForm.toEditableAuthConfig >> EditableConfig.updateAuthentication
                    , formValidation = AuthenticationConfigForm.validation appState
                    }

                ( genericModel, cmd ) =
                    GenericUpdate.update updateProps (wrapMsg << GenericMsg) genericMsg appState model.genericModel
            in
            ( { model | genericModel = genericModel }, cmd )

        GetOpenIDPrefabsComplete result ->
            case result of
                Ok openIDPrefabs ->
                    ( { model | openIDPrefabs = Success (List.map .content openIDPrefabs) }, Cmd.none )

                Err _ ->
                    ( { model | openIDPrefabs = Error "" }, Cmd.none )

        FillOpenIDServiceConfig i openIDServiceConfig ->
            let
                genericModel =
                    model.genericModel
            in
            ( { model | genericModel = { genericModel | form = AuthenticationConfigForm.fillOpenIDServiceConfig appState i openIDServiceConfig genericModel.form } }
            , Cmd.none
            )

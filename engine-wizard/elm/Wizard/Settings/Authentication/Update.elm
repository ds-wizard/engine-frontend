module Wizard.Settings.Authentication.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Shared.Api.Prefabs as PrefabsApi
import Shared.Data.EditableConfig as EditableConfig
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Settings.Authentication.Models exposing (Model)
import Wizard.Settings.Authentication.Msgs exposing (Msg(..))
import Wizard.Settings.Common.Forms.AuthenticationConfigForm as AuthenticationConfigForm
import Wizard.Settings.Generic.Update as GenericUpdate


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

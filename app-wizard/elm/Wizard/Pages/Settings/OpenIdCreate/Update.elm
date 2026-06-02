module Wizard.Pages.Settings.OpenIdCreate.Update exposing
    ( UpdateConfig
    , fetchData
    , update
    )

import ActionResult
import Common.Api.ApiError as ApiError
import Common.Ports.Dom as Dom
import Common.Ports.FormUtils as FormUtils
import Common.Ports.Window as Window
import Common.Utils.RequestHelpers as RequestHelpers
import Form
import Gettext exposing (gettext)
import Uuid
import Wizard.Api.OpenIdClients as OpenIdClientsApi
import Wizard.Api.Prefabs as PrefabsApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Settings.Common.Forms.OpenIdClientForm as OpenIdClientForm
import Wizard.Pages.Settings.OpenIdCreate.Models exposing (Model)
import Wizard.Pages.Settings.OpenIdCreate.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing as Routes


fetchData : AppState -> Cmd Msg
fetchData appState =
    Cmd.batch
        [ PrefabsApi.getOpenIDPrefabs appState GetOpenIdPrefabsComplete
        , Dom.focus "#name"
        ]


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetOpenIdPrefabsComplete result ->
            case result of
                Ok openIDPrefabs ->
                    ( { model | openIdPrefabs = ActionResult.Success (List.map .content openIDPrefabs) }, Cmd.none )

                Err err ->
                    ( { model | openIdPrefabs = ApiError.toActionResult appState (gettext "Unable to get OpenID prefabs." appState.locale) err }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        PostOpenIdComplete result ->
            let
                ( newResult, cmd ) =
                    case result of
                        Ok openId ->
                            ( ActionResult.Success ()
                            , Routes.cmdNavigate appState (Routes.settingsOpenIdDetail openId.uuid)
                            )

                        Err error ->
                            ( ApiError.toActionResult appState (gettext "OpenID config could not be saved." appState.locale) error
                            , Cmd.batch
                                [ RequestHelpers.getResultCmd cfg.logoutMsg result
                                , Dom.scrollToTop ".container"
                                ]
                            )
            in
            ( { model | savingForm = newResult }, cmd )

        Cancel ->
            ( model, Window.historyBack (Routes.toUrl Routes.settingsOpenId) )

        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just form ) ->
                    let
                        newOpenId =
                            OpenIdClientForm.toOpenIdClientDetail Uuid.nil form

                        cmd =
                            Cmd.map cfg.wrapMsg <|
                                OpenIdClientsApi.postOpenIdClient appState newOpenId PostOpenIdComplete
                    in
                    ( model, cmd )

                _ ->
                    let
                        form =
                            Form.update (OpenIdClientForm.validation appState) formMsg model.form
                    in
                    ( { model | form = form }
                    , FormUtils.scrollToInvalidField formMsg
                    )

        FillOpenIDServiceConfig openIDServiceConfig ->
            ( { model | form = OpenIdClientForm.fillFromDetail appState openIDServiceConfig model.form }
            , Cmd.none
            )

        SetAdvancedConfigExpanded expanded ->
            ( { model | advancedConfigExpanded = expanded }
            , Cmd.none
            )

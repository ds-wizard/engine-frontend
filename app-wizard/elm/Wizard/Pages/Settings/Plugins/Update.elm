module Wizard.Pages.Settings.Plugins.Update exposing
    ( fetchData
    , update
    )

import ActionResult
import Common.Api.ApiError as ApiError
import Common.Ports.Window as Window
import Dict
import Gettext exposing (gettext)
import Wizard.Api.Tenants as TenantsApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Settings.Plugins.Models exposing (Model)
import Wizard.Pages.Settings.Plugins.Msgs exposing (Msg(..))


fetchData : AppState -> Cmd Msg
fetchData _ =
    Cmd.none


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        SetPluginEnabled pluginId enabled ->
            let
                updatedPluginsEnabled =
                    Dict.insert pluginId enabled model.pluginsEnabled
            in
            ( { model | pluginsEnabled = updatedPluginsEnabled, pluginsChanged = True }
            , Cmd.none
            )

        SubmitPluginsEnabled ->
            ( { model | savingPluginsEnabled = ActionResult.Loading }
            , TenantsApi.putCurrentPluginSettings appState model.pluginsEnabled (wrapMsg << SubmitPluginsEnabledCompleted)
            )

        SubmitPluginsEnabledCompleted result ->
            case result of
                Ok _ ->
                    ( model, Window.refresh () )

                Err error ->
                    ( { model | savingPluginsEnabled = ApiError.toActionResult appState (gettext "Unable to save plugin settings." appState.locale) error }
                    , Cmd.none
                    )

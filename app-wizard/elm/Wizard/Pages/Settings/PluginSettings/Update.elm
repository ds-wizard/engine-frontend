module Wizard.Pages.Settings.PluginSettings.Update exposing
    ( fetchData
    , update
    )

import ActionResult
import Common.Api.ApiError as ApiError
import Common.Ports.Window as Window
import Gettext exposing (gettext)
import Uuid exposing (Uuid)
import Wizard.Api.Tenants as TenantsApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Settings.PluginSettings.Model exposing (Model)
import Wizard.Pages.Settings.PluginSettings.Msgs exposing (Msg(..))


fetchData : AppState -> Uuid -> Cmd Msg
fetchData appState pluginUuid =
    TenantsApi.getCurrentPluginSettingsDetail appState pluginUuid GetPluginSettingsCompleted


update : AppState -> Msg -> Model -> ( Model, Cmd Msg )
update appState msg model =
    case msg of
        GetPluginSettingsCompleted result ->
            case result of
                Ok settings ->
                    ( { model
                        | pluginSettings = ActionResult.Success settings
                        , currentPluginSettings = settings
                      }
                    , Cmd.none
                    )

                Err error ->
                    if ApiError.isNotFound error then
                        ( { model
                            | pluginSettings = ActionResult.Success ""
                            , currentPluginSettings = ""
                          }
                        , Cmd.none
                        )

                    else
                        ( { model | pluginSettings = ApiError.toActionResult appState (gettext "Unable to get plugin settings." appState.locale) error }, Cmd.none )

        UpdatePluginSettings newSettings ->
            ( { model | currentPluginSettings = newSettings }
            , Cmd.none
            )

        SavePluginSettings ->
            let
                cmd =
                    TenantsApi.putCurrentPluginSettingsDetail appState model.pluginUuid model.currentPluginSettings SavePluginSettingsCompleted
            in
            ( { model | savingPluginSettings = ActionResult.Loading }, cmd )

        SavePluginSettingsCompleted result ->
            case result of
                Ok _ ->
                    ( model
                    , Window.refresh ()
                    )

                Err error ->
                    ( { model | savingPluginSettings = ApiError.toActionResult appState (gettext "Unable to save plugin settings." appState.locale) error }
                    , Cmd.none
                    )

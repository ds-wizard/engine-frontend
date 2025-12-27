module Wizard.Pages.Settings.Plugins.Update exposing (fetchData, update)

import ActionResult
import Common.Api.ApiError as ApiError
import Common.Ports.Window as Window
import Dict
import Gettext exposing (gettext)
import Wizard.Api.Tenants as TenantsApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Settings.Plugins.Models exposing (Model, resetPluginSettings)
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

        OpenPluginSettings pluginUuid pluginSettingsElement ->
            let
                cmd =
                    TenantsApi.getCurrentPluginSettingsDetail appState pluginUuid (wrapMsg << GetPluginSettingsCompleted pluginUuid)
            in
            ( { model
                | pluginSettings = ActionResult.Loading
                , pluginSettingsUuid = Just pluginUuid
                , pluginSettingsElement = Just pluginSettingsElement
              }
            , cmd
            )

        GetPluginSettingsCompleted pluginUuid result ->
            if model.pluginSettingsUuid /= Just pluginUuid then
                -- Ignore results for plugins that are no longer open
                ( model, Cmd.none )

            else
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
            case model.pluginSettingsUuid of
                Just pluginUuid ->
                    let
                        cmd =
                            TenantsApi.putCurrentPluginSettingsDetail appState pluginUuid model.currentPluginSettings (wrapMsg << SavePluginSettingsCompleted)
                    in
                    ( { model | savingPluginSettings = ActionResult.Loading }, cmd )

                Nothing ->
                    ( model, Cmd.none )

        SavePluginSettingsCompleted result ->
            case result of
                Ok _ ->
                    ( resetPluginSettings model
                    , Cmd.none
                    )

                Err error ->
                    ( { model | savingPluginSettings = ApiError.toActionResult appState (gettext "Unable to save plugin settings." appState.locale) error }
                    , Cmd.none
                    )

        ClosePluginSettings ->
            ( { model
                | pluginSettings = ActionResult.Unset
                , pluginSettingsUuid = Nothing
                , pluginSettingsElement = Nothing
              }
            , Cmd.none
            )

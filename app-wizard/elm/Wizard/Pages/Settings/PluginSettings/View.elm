module Wizard.Pages.Settings.PluginSettings.View exposing (view)

import ActionResult
import Common.Components.Form as Form
import Common.Components.Page as Page
import Common.Utils.ShortcutUtils as Shortcut
import Gettext exposing (gettext)
import Html exposing (Html)
import Shortcut
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Settings.PluginSettings.Model exposing (Model)
import Wizard.Pages.Settings.PluginSettings.Msgs exposing (Msg(..))
import Wizard.Plugins.Plugin exposing (Plugin, SettingsConnector)
import Wizard.Plugins.PluginElement as PluginElement


view : AppState -> Model -> Html Msg
view appState model =
    let
        mbPlugin =
            AppState.getPlugin appState model.pluginUuid

        mbSettingsConnector =
            Maybe.andThen (\plugin -> plugin.connectors.settings) mbPlugin
    in
    case ( mbPlugin, mbSettingsConnector ) of
        ( Just plugin, Just settingsConnector ) ->
            Page.actionResultView appState (viewPluginSettings appState model plugin settingsConnector) model.pluginSettings

        _ ->
            Page.error appState (gettext "Plugin not found" appState.locale)


viewPluginSettings : AppState -> Model -> Plugin -> SettingsConnector -> String -> Html Msg
viewPluginSettings appState model plugin settingsConnector pluginSettings =
    let
        formChanged =
            ActionResult.isSuccess model.pluginSettings && (ActionResult.Success model.currentPluginSettings /= model.pluginSettings)

        shortcuts =
            if not (ActionResult.isLoading model.savingPluginSettings) && formChanged then
                [ Shortcut.submitShortcut appState.navigator.isMac SavePluginSettings ]

            else
                []

        formActions =
            Form.formActionsDynamic
                { submitMsg = SavePluginSettings
                , actionResult = model.savingPluginSettings
                , formChanged = ActionResult.isSuccess model.pluginSettings && (ActionResult.Success model.currentPluginSettings /= model.pluginSettings)
                , wide = True
                , locale = appState.locale
                }
    in
    Shortcut.shortcutElement shortcuts
        []
        [ Page.header plugin.name []
        , PluginElement.element settingsConnector.element
            [ PluginElement.settingValue pluginSettings
            , PluginElement.onSettingsValueChange UpdatePluginSettings
            ]
        , formActions
        ]

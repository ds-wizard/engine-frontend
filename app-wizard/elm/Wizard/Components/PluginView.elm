module Wizard.Components.PluginView exposing (view)

import Html exposing (Attribute, Html)
import Uuid exposing (Uuid)
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Plugins.PluginElement as PluginElement exposing (PluginElement)


view : AppState -> Uuid -> PluginElement -> List (Attribute msg) -> Html msg
view appState pluginUuid pluginElement attributes =
    PluginElement.element pluginElement
        (PluginElement.settingValue (AppState.getPluginSettings appState pluginUuid)
            :: PluginElement.userSettingsValue (AppState.getPluginUserSettings appState pluginUuid)
            :: attributes
        )

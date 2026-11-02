module Common.Components.PluginView exposing (view)

{-| A plugin's custom element, given the settings it was configured with.

Plugins are custom elements defined by code the instance loads at start-up, so
this is the one place in the application that passes a list of attributes: the
element's interface is the plugin's, not ours, and each connector adds its own
attributes (the document, the project, callbacks).

-}

import Common.Data.AppState as AppState exposing (AppState)
import Common.Plugins.PluginElement as PluginElement exposing (PluginElement)
import Html exposing (Attribute, Html)
import Uuid exposing (Uuid)


view : AppState -> Uuid -> PluginElement -> List (Attribute msg) -> Html msg
view appState pluginUuid pluginElement attributes =
    PluginElement.element pluginElement
        (PluginElement.settingValue (AppState.getPluginSettings appState pluginUuid)
            :: PluginElement.userSettingsValue (AppState.getPluginUserSettings appState pluginUuid)
            :: PluginElement.userValue (AppState.getPluginUser appState)
            :: attributes
        )

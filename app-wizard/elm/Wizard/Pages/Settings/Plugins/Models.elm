module Wizard.Pages.Settings.Plugins.Models exposing
    ( Model
    , initialModel
    , resetPluginSettings
    )

import ActionResult exposing (ActionResult)
import Dict exposing (Dict)
import Uuid exposing (Uuid)
import Wizard.Api.Models.BootstrapConfig.PluginInfo as PluginInfo
import Wizard.Data.AppState exposing (AppState)
import Wizard.Plugins.PluginElement exposing (PluginElement)


type alias Model =
    { pluginsEnabled : Dict String Bool
    , pluginsChanged : Bool
    , savingPluginsEnabled : ActionResult ()
    , pluginSettings : ActionResult String
    , pluginSettingsUuid : Maybe Uuid
    , pluginSettingsElement : Maybe PluginElement
    , currentPluginSettings : String
    , savingPluginSettings : ActionResult String
    }


initialModel : AppState -> Model
initialModel appState =
    let
        pluginsEnabled =
            appState.config.plugins
                |> List.map PluginInfo.toTuple
                |> Dict.fromList
    in
    { pluginsEnabled = pluginsEnabled
    , pluginsChanged = False
    , savingPluginsEnabled = ActionResult.Unset
    , pluginSettings = ActionResult.Unset
    , pluginSettingsUuid = Nothing
    , pluginSettingsElement = Nothing
    , currentPluginSettings = ""
    , savingPluginSettings = ActionResult.Unset
    }


resetPluginSettings : Model -> Model
resetPluginSettings model =
    { model
        | pluginSettings = ActionResult.Unset
        , pluginSettingsUuid = Nothing
        , currentPluginSettings = ""
        , savingPluginSettings = ActionResult.Unset
    }

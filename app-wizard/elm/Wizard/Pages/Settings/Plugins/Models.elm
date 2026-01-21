module Wizard.Pages.Settings.Plugins.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult)
import Dict exposing (Dict)
import Wizard.Api.Models.BootstrapConfig.PluginInfo as PluginInfo
import Wizard.Data.AppState exposing (AppState)


type alias Model =
    { pluginsEnabled : Dict String Bool
    , pluginsChanged : Bool
    , savingPluginsEnabled : ActionResult ()
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
    }

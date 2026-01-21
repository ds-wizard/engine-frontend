module Wizard.Pages.Settings.PluginSettings.Model exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult)
import Uuid exposing (Uuid)


type alias Model =
    { pluginUuid : Uuid
    , pluginSettings : ActionResult String
    , currentPluginSettings : String
    , savingPluginSettings : ActionResult String
    }


initialModel : Uuid -> Model
initialModel pluginUuid =
    { pluginUuid = pluginUuid
    , pluginSettings = ActionResult.Loading
    , currentPluginSettings = ""
    , savingPluginSettings = ActionResult.Unset
    }

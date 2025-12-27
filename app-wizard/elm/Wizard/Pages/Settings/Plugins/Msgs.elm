module Wizard.Pages.Settings.Plugins.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Uuid exposing (Uuid)
import Wizard.Plugins.PluginElement exposing (PluginElement)


type Msg
    = SetPluginEnabled String Bool
    | SubmitPluginsEnabled
    | SubmitPluginsEnabledCompleted (Result ApiError ())
    | OpenPluginSettings Uuid PluginElement
    | GetPluginSettingsCompleted Uuid (Result ApiError String)
    | UpdatePluginSettings String
    | SavePluginSettings
    | SavePluginSettingsCompleted (Result ApiError ())
    | ClosePluginSettings

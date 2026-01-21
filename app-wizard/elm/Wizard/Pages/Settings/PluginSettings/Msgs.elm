module Wizard.Pages.Settings.PluginSettings.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)


type Msg
    = GetPluginSettingsCompleted (Result ApiError String)
    | UpdatePluginSettings String
    | SavePluginSettings
    | SavePluginSettingsCompleted (Result ApiError ())

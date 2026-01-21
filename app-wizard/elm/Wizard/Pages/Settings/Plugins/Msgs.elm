module Wizard.Pages.Settings.Plugins.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)


type Msg
    = SetPluginEnabled String Bool
    | SubmitPluginsEnabled
    | SubmitPluginsEnabledCompleted (Result ApiError ())

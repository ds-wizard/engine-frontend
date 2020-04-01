module Wizard.Common.Api.Configs exposing (..)

import Json.Encode as E
import Wizard.Common.Api exposing (ToMsg, jwtGet, jwtPut)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Settings.Common.EditableConfig as EditableConfig exposing (EditableConfig)


getAppConfig : AppState -> ToMsg EditableConfig msg -> Cmd msg
getAppConfig =
    jwtGet "/configs/app" EditableConfig.decoder


putAppConfig : E.Value -> AppState -> ToMsg () msg -> Cmd msg
putAppConfig =
    jwtPut "/configs/app"

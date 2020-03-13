module Wizard.Common.Api.Config exposing (..)

import Json.Encode as E
import Wizard.Common.Api exposing (ToMsg, jwtGet, jwtPut)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Settings.Common.EditableConfig as EditableConfig exposing (EditableConfig)


getApplicationConfig : AppState -> ToMsg EditableConfig msg -> Cmd msg
getApplicationConfig =
    jwtGet "/configs/application" EditableConfig.decoder


putApplicationConfig : E.Value -> AppState -> ToMsg () msg -> Cmd msg
putApplicationConfig =
    jwtPut "/configs/application"

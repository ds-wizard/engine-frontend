module Shared.Api.Configs exposing
    ( getAppConfig
    , putAppConfig
    )

import Json.Encode as E
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtGet, jwtPut)
import Shared.Data.EditableConfig as EditableConfig exposing (EditableConfig)


getAppConfig : AbstractAppState a -> ToMsg EditableConfig msg -> Cmd msg
getAppConfig =
    jwtGet "/configs/app" EditableConfig.decoder


putAppConfig : E.Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putAppConfig =
    jwtPut "/configs/app"

module Shared.Api.Configs exposing
    ( deleteLogo
    , getAppConfig
    , putAppConfig
    , uploadLogo
    )

import File exposing (File)
import Json.Encode as E
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtDelete, jwtGet, jwtPostFile, jwtPut)
import Shared.Data.EditableConfig as EditableConfig exposing (EditableConfig)


getAppConfig : AbstractAppState a -> ToMsg EditableConfig msg -> Cmd msg
getAppConfig =
    jwtGet "/configs/app" EditableConfig.decoder


putAppConfig : E.Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putAppConfig =
    jwtPut "/configs/app"


uploadLogo : File -> AbstractAppState a -> ToMsg () msg -> Cmd msg
uploadLogo =
    jwtPostFile "/configs/app/logo"


deleteLogo : AbstractAppState a -> ToMsg () msg -> Cmd msg
deleteLogo =
    jwtDelete "/configs/app/logo"

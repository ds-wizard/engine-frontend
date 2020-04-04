module Shared.Api.Users exposing (..)

import Shared.Api exposing (AppStateLike, ToMsg, jwtGet)
import Shared.Data.UserInfo as UserInfo exposing (UserInfo)


getUserInfo : AppStateLike a -> ToMsg UserInfo msg -> Cmd msg
getUserInfo =
    jwtGet "/users/current" UserInfo.decoder

module Shared.Api.Templates exposing (..)

import Json.Decode as D
import Shared.Api exposing (AppStateLike, ToMsg, jwtGet)
import Shared.Data.Template as Template exposing (Template)


getTemplates : AppStateLike a -> ToMsg (List Template) msg -> Cmd msg
getTemplates =
    jwtGet "/templates" (D.list Template.decoder)

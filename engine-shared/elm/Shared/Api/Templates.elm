module Shared.Api.Templates exposing
    ( getTemplates
    , getTemplatesFor
    )

import Json.Decode as D
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtGet)
import Shared.Data.Template as Template exposing (Template)


getTemplates : AbstractAppState a -> ToMsg (List Template) msg -> Cmd msg
getTemplates =
    jwtGet "/templates" (D.list Template.decoder)


getTemplatesFor : String -> AbstractAppState a -> ToMsg (List Template) msg -> Cmd msg
getTemplatesFor pkgId =
    jwtGet ("/templates?pkgId=" ++ pkgId) (D.list Template.decoder)

module Wizard.Common.Api.Templates exposing (..)

import Wizard.Common.Api exposing (ToMsg, jwtGet)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Documents.Common.Template as Template exposing (Template)


getTemplates : AppState -> ToMsg (List Template) msg -> Cmd msg
getTemplates =
    jwtGet "/templates" Template.listDecoder


getTemplatesFor : String -> AppState -> ToMsg (List Template) msg -> Cmd msg
getTemplatesFor pkgId =
    jwtGet ("/templates?pkgId=" ++ pkgId) Template.listDecoder

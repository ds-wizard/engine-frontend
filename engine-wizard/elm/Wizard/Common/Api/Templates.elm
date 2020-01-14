module Wizard.Common.Api.Templates exposing (getTemplatesFor)

import Wizard.Common.Api exposing (ToMsg, jwtGet)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Questionnaires.Common.Template as Template exposing (Template)


getTemplatesFor : String -> AppState -> ToMsg (List Template) msg -> Cmd msg
getTemplatesFor pkgId =
    jwtGet ("/templates?pkgId=" ++ pkgId) Template.listDecoder

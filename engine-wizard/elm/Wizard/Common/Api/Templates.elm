module Wizard.Common.Api.Templates exposing (getTemplates)

import Wizard.Common.Api exposing (ToMsg, jwtGet)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Questionnaires.Common.Template as Template exposing (Template)


getTemplates : AppState -> ToMsg (List Template) msg -> Cmd msg
getTemplates =
    jwtGet "/templates" Template.listDecoder

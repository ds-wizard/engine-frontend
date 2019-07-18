module Common.Api.Templates exposing (getTemplates)

import Common.Api exposing (ToMsg, jwtGet)
import Common.AppState exposing (AppState)
import Questionnaires.Common.Template as Template exposing (Template)


getTemplates : AppState -> ToMsg (List Template) msg -> Cmd msg
getTemplates =
    jwtGet "/templates" Template.listDecoder

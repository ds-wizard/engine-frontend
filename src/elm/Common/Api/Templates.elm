module Common.Api.Templates exposing (getTemplates)

import Common.Api exposing (ToMsg, jwtGet)
import Common.AppState exposing (AppState)
import Questionnaires.Index.ExportModal.Models exposing (Template, templateListDecoder)


getTemplates : AppState -> ToMsg (List Template) msg -> Cmd msg
getTemplates =
    jwtGet "/templates" templateListDecoder

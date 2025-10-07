module Wizard.Api.Models.WebSockets.QuestionnaireAction.SetQuestionnaireData exposing
    ( SetQuestionnaireData
    , decoder
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)
import Wizard.Api.Models.DocumentTemplate.DocumentTemplateFormat as DocumentTemplateFormat exposing (DocumentTemplateFormat)
import Wizard.Api.Models.DocumentTemplateSuggestion as DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)
import Wizard.Api.Models.Permission as Permission exposing (Permission)
import Wizard.Api.Models.Questionnaire.QuestionnaireSharing as QuestionnaireSharing exposing (QuestionnaireSharing)
import Wizard.Api.Models.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility)


type alias SetQuestionnaireData =
    { name : String
    , description : Maybe String
    , projectTags : List String
    , isTemplate : Bool
    , visibility : QuestionnaireVisibility
    , sharing : QuestionnaireSharing
    , documentTemplateId : Maybe String
    , documentTemplate : Maybe DocumentTemplateSuggestion
    , formatUuid : Maybe Uuid
    , format : Maybe DocumentTemplateFormat
    , permissions : List Permission
    , labels : Dict String (List String)
    , unresolvedCommentCounts : Dict String (Dict String Int)
    , resolvedCommentCounts : Dict String (Dict String Int)
    }


decoder : Decoder SetQuestionnaireData
decoder =
    D.succeed SetQuestionnaireData
        |> D.required "name" D.string
        |> D.required "description" (D.maybe D.string)
        |> D.required "projectTags" (D.list D.string)
        |> D.required "isTemplate" D.bool
        |> D.required "visibility" QuestionnaireVisibility.decoder
        |> D.required "sharing" QuestionnaireSharing.decoder
        |> D.required "documentTemplateId" (D.maybe D.string)
        |> D.required "documentTemplate" (D.maybe DocumentTemplateSuggestion.decoder)
        |> D.required "formatUuid" (D.maybe Uuid.decoder)
        |> D.required "format" (D.maybe DocumentTemplateFormat.decoder)
        |> D.required "permissions" (D.list Permission.decoder)
        |> D.required "labels" (D.dict (D.list D.string))
        |> D.required "unresolvedCommentCounts" (D.dict (D.dict D.int))
        |> D.required "resolvedCommentCounts" (D.dict (D.dict D.int))

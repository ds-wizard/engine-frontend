module Shared.Data.WebSockets.QuestionnaireAction.SetQuestionnaireData exposing
    ( SetQuestionnaireData
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.DocumentTemplate.DocumentTemplateFormat as DocumentTemplateFormat exposing (DocumentTemplateFormat)
import Shared.Data.DocumentTemplateSuggestion as DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)
import Shared.Data.Permission as Permission exposing (Permission)
import Shared.Data.Questionnaire.QuestionnaireSharing as QuestionnaireSharing exposing (QuestionnaireSharing)
import Shared.Data.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility)
import Uuid exposing (Uuid)


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

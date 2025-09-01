module Wizard.Api.Models.QuestionnairePreview exposing
    ( QuestionnairePreview
    , decoder
    , hasTemplateSet
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)
import Wizard.Api.Models.DocumentTemplate.DocumentTemplateFormat as DocumentTemplateFormat exposing (DocumentTemplateFormat)
import Wizard.Api.Models.Permission as Permission exposing (Permission)
import Wizard.Api.Models.Questionnaire.QuestionnaireSharing as QuestionnaireSharing exposing (QuestionnaireSharing)
import Wizard.Api.Models.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility)


type alias QuestionnairePreview =
    { uuid : Uuid
    , documentTemplateId : Maybe String
    , format : Maybe DocumentTemplateFormat
    , permissions : List Permission
    , sharing : QuestionnaireSharing
    , visibility : QuestionnaireVisibility
    , migrationUuid : Maybe Uuid
    }


decoder : Decoder QuestionnairePreview
decoder =
    D.succeed QuestionnairePreview
        |> D.required "uuid" Uuid.decoder
        |> D.required "documentTemplateId" (D.maybe D.string)
        |> D.required "format" (D.maybe DocumentTemplateFormat.decoder)
        |> D.required "permissions" (D.list Permission.decoder)
        |> D.required "sharing" QuestionnaireSharing.decoder
        |> D.required "visibility" QuestionnaireVisibility.decoder
        |> D.required "migrationUuid" (D.maybe Uuid.decoder)


hasTemplateSet : QuestionnairePreview -> Bool
hasTemplateSet questionnairePreview =
    Maybe.isJust questionnairePreview.documentTemplateId && Maybe.isJust questionnairePreview.format

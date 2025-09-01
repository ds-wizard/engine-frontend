module Wizard.Api.Models.Document exposing
    ( Document
    , decoder
    , isOwner
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Uuid exposing (Uuid)
import Wizard.Api.Models.Document.DocumentState as DocumentState exposing (DocumentState)
import Wizard.Api.Models.DocumentTemplate.DocumentTemplateFormat as DocumentTemplateFormat exposing (DocumentTemplateFormat)
import Wizard.Api.Models.QuestionnaireInfo as QuestionnaireInfo exposing (QuestionnaireInfo)
import Wizard.Api.Models.Submission as Submission exposing (Submission)
import Wizard.Common.AppState exposing (AppState)


type alias Document =
    { uuid : Uuid
    , name : String
    , createdAt : Time.Posix
    , questionnaire : Maybe QuestionnaireInfo
    , questionnaireEventUuid : Maybe Uuid
    , questionnaireVersion : Maybe String
    , documentTemplateId : String
    , documentTemplateName : String
    , format : Maybe DocumentTemplateFormat
    , state : DocumentState
    , submissions : List Submission
    , createdBy : Maybe Uuid
    , fileSize : Maybe Int
    , workerLog : Maybe String
    }


isOwner : AppState -> Document -> Bool
isOwner appState document =
    appState.config.user
        |> Maybe.map (.uuid >> Just >> (==) document.createdBy)
        |> Maybe.withDefault False


decoder : Decoder Document
decoder =
    D.succeed Document
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "createdAt" D.datetime
        |> D.optional "questionnaire" (D.maybe QuestionnaireInfo.decoder) Nothing
        |> D.required "questionnaireEventUuid" (D.maybe Uuid.decoder)
        |> D.required "questionnaireVersion" (D.maybe D.string)
        |> D.required "documentTemplateId" D.string
        |> D.required "documentTemplateName" D.string
        |> D.required "format" (D.maybe DocumentTemplateFormat.decoder)
        |> D.required "state" DocumentState.decoder
        |> D.required "submissions" (D.list Submission.decoder)
        |> D.required "createdBy" (D.maybe Uuid.decoder)
        |> D.required "fileSize" (D.maybe D.int)
        |> D.required "workerLog" (D.maybe D.string)

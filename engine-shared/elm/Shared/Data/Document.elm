module Shared.Data.Document exposing
    ( Document
    , decoder
    , isOwner
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Data.Document.DocumentState as DocumentState exposing (DocumentState)
import Shared.Data.DocumentTemplate.DocumentTemplateFormat as DocumentTemplateFormat exposing (DocumentTemplateFormat)
import Shared.Data.QuestionnaireInfo as QuestionnaireInfo exposing (QuestionnaireInfo)
import Shared.Data.Submission as Submission exposing (Submission)
import Time
import Uuid exposing (Uuid)


type alias Document =
    { uuid : Uuid
    , name : String
    , createdAt : Time.Posix
    , questionnaire : Maybe QuestionnaireInfo
    , questionnaireEventUuid : Maybe Uuid
    , questionnaireVersion : Maybe String
    , documentTemplateName : String
    , format : Maybe DocumentTemplateFormat
    , state : DocumentState
    , submissions : List Submission
    , createdBy : Maybe Uuid
    , fileSize : Maybe Int
    , workerLog : Maybe String
    }


isOwner : AbstractAppState a -> Document -> Bool
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
        |> D.required "documentTemplateName" D.string
        |> D.required "format" (D.maybe DocumentTemplateFormat.decoder)
        |> D.required "state" DocumentState.decoder
        |> D.required "submissions" (D.list Submission.decoder)
        |> D.required "createdBy" (D.maybe Uuid.decoder)
        |> D.required "fileSize" (D.maybe D.int)
        |> D.required "workerLog" (D.maybe D.string)

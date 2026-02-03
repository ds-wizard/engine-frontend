module Wizard.Api.Models.Document exposing
    ( Document
    , decoder
    , encode
    , isOwner
    )

import Iso8601
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Time
import Uuid exposing (Uuid)
import Wizard.Api.Models.BootstrapConfig exposing (BootstrapConfig)
import Wizard.Api.Models.Document.DocumentState as DocumentState exposing (DocumentState)
import Wizard.Api.Models.DocumentTemplate.DocumentTemplateFormat as DocumentTemplateFormat exposing (DocumentTemplateFormat)
import Wizard.Api.Models.ProjectInfo as ProjectInfo exposing (ProjectInfo)
import Wizard.Api.Models.Submission as Submission exposing (Submission)


type alias Document =
    { uuid : Uuid
    , name : String
    , createdAt : Time.Posix
    , project : Maybe ProjectInfo
    , projectEventUuid : Maybe Uuid
    , projectVersion : Maybe String
    , documentTemplateId : String
    , documentTemplateName : String
    , format : DocumentTemplateFormat
    , state : DocumentState
    , submissions : List Submission
    , createdBy : Maybe Uuid
    , fileSize : Maybe Int
    , workerLog : Maybe String
    }


isOwner : { a | config : BootstrapConfig } -> Document -> Bool
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
        |> D.optional "project" (D.maybe ProjectInfo.decoder) Nothing
        |> D.required "projectEventUuid" (D.maybe Uuid.decoder)
        |> D.required "projectVersion" (D.maybe D.string)
        |> D.required "documentTemplateId" D.string
        |> D.required "documentTemplateName" D.string
        |> D.required "format" DocumentTemplateFormat.decoder
        |> D.required "state" DocumentState.decoder
        |> D.required "submissions" (D.list Submission.decoder)
        |> D.required "createdBy" (D.maybe Uuid.decoder)
        |> D.required "fileSize" (D.maybe D.int)
        |> D.required "workerLog" (D.maybe D.string)


encode : Document -> E.Value
encode document =
    E.object
        [ ( "uuid", Uuid.encode document.uuid )
        , ( "name", E.string document.name )
        , ( "createdAt", Iso8601.encode document.createdAt )
        , ( "project", E.maybe ProjectInfo.encode document.project )
        , ( "projectEventUuid", E.maybe Uuid.encode document.projectEventUuid )
        , ( "projectVersion", E.maybe E.string document.projectVersion )
        , ( "documentTemplateId", E.string document.documentTemplateId )
        , ( "documentTemplateName", E.string document.documentTemplateName )
        , ( "format", DocumentTemplateFormat.encode document.format )
        , ( "state", DocumentState.encode document.state )
        , ( "createdBy", E.maybe Uuid.encode document.createdBy )
        , ( "fileSize", E.maybe E.int document.fileSize )
        , ( "workerLog", E.maybe E.string document.workerLog )
        ]

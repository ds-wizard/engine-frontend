module Common.Api.Models.DocumentTemplate exposing
    ( DocumentTemplate
    , decoder
    , isOutdated
    )

import Common.Api.Models.DocumentTemplate.DocumentTemplatePhase as DocumentTemplatePhase exposing (DocumentTemplatePhase)
import Common.Api.Models.DocumentTemplate.DocumentTemplateState as DocumentTemplateState exposing (DocumentTemplateState)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Uuid exposing (Uuid)
import Version exposing (Version)


type alias DocumentTemplate =
    { createdAt : Time.Posix
    , description : String
    , uuid : Uuid
    , language : String
    , name : String
    , phase : DocumentTemplatePhase
    , remoteLatestVersion : Maybe Version
    , state : DocumentTemplateState
    , id : String
    , version : Version
    , nonEditable : Bool
    , potFileReady : Bool
    , workspaceUuid : Maybe Uuid
    }


decoder : Decoder DocumentTemplate
decoder =
    D.succeed DocumentTemplate
        |> D.required "createdAt" D.datetime
        |> D.required "description" D.string
        |> D.required "uuid" Uuid.decoder
        |> D.required "language" D.string
        |> D.required "name" D.string
        |> D.required "phase" DocumentTemplatePhase.decoder
        |> D.required "remoteLatestVersion" (D.maybe Version.decoder)
        |> D.required "state" DocumentTemplateState.decoder
        |> D.required "id" D.string
        |> D.required "version" Version.decoder
        |> D.required "nonEditable" D.bool
        |> D.required "potFileReady" D.bool
        |> D.optional "workspaceUuid" (D.nullable Uuid.decoder) Nothing


isOutdated : { a | remoteLatestVersion : Maybe Version, version : Version } -> Bool
isOutdated template =
    case template.remoteLatestVersion of
        Just remoteLatestVersion ->
            Version.greaterThan template.version remoteLatestVersion

        Nothing ->
            False

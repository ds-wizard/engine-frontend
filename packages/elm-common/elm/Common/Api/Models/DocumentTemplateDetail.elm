module Common.Api.Models.DocumentTemplateDetail exposing
    ( DocumentTemplateDetail
    , decoder
    , encode
    , isLatestVersion
    )

import Common.Api.Models.DocumentTemplate.DocumentTemplateAllowedPackage as DocumentTemplateAllowedPackage exposing (DocumentTemplateAllowedPackage)
import Common.Api.Models.DocumentTemplate.DocumentTemplateFormat as DocumentTemplateFormat exposing (DocumentTemplateFormat)
import Common.Api.Models.DocumentTemplate.DocumentTemplatePackage as DocumentTemplatePackage exposing (DocumentTemplatePackage)
import Common.Api.Models.DocumentTemplate.DocumentTemplatePhase as DocumentTemplatePhase exposing (DocumentTemplatePhase)
import Common.Api.Models.DocumentTemplate.DocumentTemplateState as DocumentTemplateState exposing (DocumentTemplateState)
import Common.Api.Models.DocumentTemplateLocale as DocumentTemplateLocale exposing (DocumentTemplateLocale)
import Common.Api.Models.VersionUuid as VersionUuid exposing (VersionUuid)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Time
import Uuid exposing (Uuid)
import Version exposing (Version)


type alias DocumentTemplateDetail =
    { allowedPackages : List DocumentTemplateAllowedPackage
    , createdAt : Time.Posix
    , description : String
    , formats : List DocumentTemplateFormat
    , uuid : Uuid
    , language : String
    , license : String
    , locales : List DocumentTemplateLocale
    , metamodelVersion : Version
    , name : String
    , phase : DocumentTemplatePhase
    , readme : String
    , registryLink : Maybe String
    , remoteLatestVersion : Maybe Version
    , state : DocumentTemplateState
    , id : String
    , usableKnowledgeModels : List DocumentTemplatePackage
    , version : Version
    , versions : List VersionUuid
    , nonEditable : Bool
    , potFileReady : Bool
    , workspaceUuid : Maybe Uuid
    }


decoder : Decoder DocumentTemplateDetail
decoder =
    D.succeed DocumentTemplateDetail
        |> D.required "allowedPackages" (D.list DocumentTemplateAllowedPackage.decoder)
        |> D.required "createdAt" D.datetime
        |> D.required "description" D.string
        |> D.required "formats" (D.list DocumentTemplateFormat.decoder)
        |> D.required "uuid" Uuid.decoder
        |> D.required "language" D.string
        |> D.required "license" D.string
        |> D.required "locales" (D.list DocumentTemplateLocale.decoder)
        |> D.required "metamodelVersion" Version.decoder
        |> D.required "name" D.string
        |> D.required "phase" DocumentTemplatePhase.decoder
        |> D.required "readme" D.string
        |> D.required "registryLink" (D.maybe D.string)
        |> D.required "remoteLatestVersion" (D.maybe Version.decoder)
        |> D.required "state" DocumentTemplateState.decoder
        |> D.required "id" D.string
        |> D.required "usableKnowledgeModels" (D.list DocumentTemplatePackage.decoder)
        |> D.required "version" Version.decoder
        |> D.required "versions" (D.list VersionUuid.decoder)
        |> D.required "nonEditable" D.bool
        |> D.required "potFileReady" D.bool
        |> D.optional "workspaceUuid" (D.nullable Uuid.decoder) Nothing


encode : { a | phase : DocumentTemplatePhase } -> E.Value
encode documentTemplate =
    E.object
        [ ( "phase", DocumentTemplatePhase.encode documentTemplate.phase ) ]


isLatestVersion : DocumentTemplateDetail -> Bool
isLatestVersion documentTemplate =
    documentTemplate.versions
        |> List.any (Version.greaterThan documentTemplate.version << .version)
        |> not

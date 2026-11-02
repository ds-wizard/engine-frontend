module Common.Api.Models.KnowledgeModelPackageDetail exposing
    ( KnowledgeModelPackageDetail
    , createFormOptions
    , decoder
    , encode
    , getLatestPackageUuid
    , toPackage
    , toPackageSuggestion
    )

import Common.Api.Models.KnowledgeModelLocale as KnowledgeModelLocale exposing (KnowledgeModelLocale)
import Common.Api.Models.KnowledgeModelPackage exposing (KnowledgeModelPackage)
import Common.Api.Models.KnowledgeModelPackage.KnowledgeModelPackagePhase as KnoweldgeModelPackagePhase exposing (KnowledgeModelPackagePhase)
import Common.Api.Models.KnowledgeModelPackageSuggestion exposing (KnowledgeModelPackageSuggestion)
import Common.Api.Models.VersionUuid as VersionUuid exposing (VersionUuid)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import List.Extra as List
import Time
import Uuid exposing (Uuid)
import Version exposing (Version)


type alias KnowledgeModelPackageDetail =
    { uuid : Uuid
    , name : String
    , id : String
    , version : Version
    , description : String
    , readme : String
    , license : String
    , language : String
    , locales : List KnowledgeModelLocale
    , metamodelVersion : Int
    , forkOfPackageId : Maybe String
    , forkOfPackageVersion : Maybe Version
    , previousPackageUuid : Maybe Uuid
    , versions : List VersionUuid
    , registryLink : Maybe String
    , remoteLatestVersion : Maybe Version
    , phase : KnowledgeModelPackagePhase
    , createdAt : Time.Posix
    , nonEditable : Bool
    , public : Bool
    , workspaceUuid : Maybe Uuid
    }


decoder : Decoder KnowledgeModelPackageDetail
decoder =
    D.succeed KnowledgeModelPackageDetail
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "id" D.string
        |> D.required "version" Version.decoder
        |> D.required "description" D.string
        |> D.required "readme" D.string
        |> D.required "license" D.string
        |> D.required "language" D.string
        |> D.required "locales" (D.list KnowledgeModelLocale.decoder)
        |> D.required "metamodelVersion" D.int
        |> D.required "forkOfPackageId" (D.maybe D.string)
        |> D.optional "forkOfPackageVersion" (D.maybe Version.decoder) Nothing
        |> D.required "previousPackageUuid" (D.maybe Uuid.decoder)
        |> D.required "versions" (D.list VersionUuid.decoder)
        |> D.required "registryLink" (D.maybe D.string)
        |> D.required "remoteLatestVersion" (D.maybe Version.decoder)
        |> D.required "phase" KnoweldgeModelPackagePhase.decoder
        |> D.required "createdAt" D.datetime
        |> D.required "nonEditable" D.bool
        |> D.required "public" D.bool
        |> D.optional "workspaceUuid" (D.nullable Uuid.decoder) Nothing


encode : { a | phase : KnowledgeModelPackagePhase, public : Bool } -> E.Value
encode kmPackage =
    E.object
        [ ( "phase", KnoweldgeModelPackagePhase.encode kmPackage.phase )
        , ( "public", E.bool kmPackage.public )
        ]


toPackage : KnowledgeModelPackageDetail -> KnowledgeModelPackage
toPackage kmPackage =
    { uuid = kmPackage.uuid
    , name = kmPackage.name
    , id = kmPackage.id
    , version = kmPackage.version
    , description = kmPackage.description
    , language = kmPackage.language
    , remoteLatestVersion = kmPackage.remoteLatestVersion
    , phase = kmPackage.phase
    , createdAt = kmPackage.createdAt
    , nonEditable = True
    , public = kmPackage.public
    , workspaceUuid = Nothing
    }


toPackageSuggestion : KnowledgeModelPackageDetail -> KnowledgeModelPackageSuggestion
toPackageSuggestion kmPackage =
    { uuid = kmPackage.uuid
    , name = kmPackage.name
    , description = kmPackage.description
    , id = kmPackage.id
    , version = kmPackage.version
    , workspaceUuid = kmPackage.workspaceUuid
    }


getLatestVersion : KnowledgeModelPackageDetail -> Maybe VersionUuid
getLatestVersion =
    List.last << List.sortWith VersionUuid.compare << .versions


getLatestPackageUuid : KnowledgeModelPackageDetail -> Maybe Uuid
getLatestPackageUuid kmPackage =
    case getLatestVersion kmPackage of
        Just latestVersion ->
            Just latestVersion.uuid

        _ ->
            Nothing


createFormOptions : KnowledgeModelPackageDetail -> List ( String, String )
createFormOptions kmPackage =
    kmPackage.versions
        |> List.filter (Version.greaterThan kmPackage.version << .version)
        |> List.sortWith VersionUuid.compare
        |> List.map (createFormOption kmPackage)


createFormOption : KnowledgeModelPackageDetail -> VersionUuid -> ( String, String )
createFormOption kmPackage version =
    let
        id =
            kmPackage.id ++ ":" ++ Version.toString version.version

        optionText =
            kmPackage.name ++ " " ++ Version.toString version.version ++ " (" ++ id ++ ")"
    in
    ( Uuid.toString version.uuid, optionText )

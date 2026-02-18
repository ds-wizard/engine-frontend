module Wizard.Api.Models.KnowledgeModelPackageDetail exposing
    ( KnowledgeModelPackageDetail
    , decoder
    , encode
    , getLatestPackageUuid
    , toPackage
    , toPackageSuggestion
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import List.Extra as List
import Time
import Uuid exposing (Uuid)
import Version exposing (Version)
import Wizard.Api.Models.KnowledgeModelPackage exposing (KnowledgeModelPackage)
import Wizard.Api.Models.KnowledgeModelPackage.KnowledgeModelPackagePhase as KnoweldgeModelPackagePhase exposing (KnowledgeModelPackagePhase)
import Wizard.Api.Models.KnowledgeModelPackageSuggestion exposing (KnowledgeModelPackageSuggestion)
import Wizard.Api.Models.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)
import Wizard.Api.Models.VersionUuid as VersionUuid exposing (VersionUuid)


type alias KnowledgeModelPackageDetail =
    { uuid : Uuid
    , name : String
    , organizationId : String
    , kmId : String
    , version : Version
    , description : String
    , readme : String
    , license : String
    , metamodelVersion : Int
    , forkOfPackageId : Maybe String
    , previousPackageUuid : Maybe Uuid
    , versions : List VersionUuid
    , organization : Maybe OrganizationInfo
    , registryLink : Maybe String
    , remoteLatestVersion : Maybe Version
    , phase : KnowledgeModelPackagePhase
    , nonEditable : Bool
    , public : Bool
    }


decoder : Decoder KnowledgeModelPackageDetail
decoder =
    D.succeed KnowledgeModelPackageDetail
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "organizationId" D.string
        |> D.required "kmId" D.string
        |> D.required "version" Version.decoder
        |> D.required "description" D.string
        |> D.required "readme" D.string
        |> D.required "license" D.string
        |> D.required "metamodelVersion" D.int
        |> D.required "forkOfPackageId" (D.maybe D.string)
        |> D.required "previousPackageUuid" (D.maybe Uuid.decoder)
        |> D.required "versions" (D.list VersionUuid.decoder)
        |> D.required "organization" (D.maybe OrganizationInfo.decoder)
        |> D.required "registryLink" (D.maybe D.string)
        |> D.required "remoteLatestVersion" (D.maybe Version.decoder)
        |> D.required "phase" KnoweldgeModelPackagePhase.decoder
        |> D.required "nonEditable" D.bool
        |> D.required "public" D.bool


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
    , organizationId = kmPackage.organizationId
    , kmId = kmPackage.kmId
    , version = kmPackage.version
    , description = kmPackage.description
    , organization = kmPackage.organization
    , remoteLatestVersion = kmPackage.remoteLatestVersion
    , phase = kmPackage.phase
    , createdAt = Time.millisToPosix 0
    , nonEditable = True
    , public = kmPackage.public
    }


toPackageSuggestion : KnowledgeModelPackageDetail -> KnowledgeModelPackageSuggestion
toPackageSuggestion kmPackage =
    { uuid = kmPackage.uuid
    , name = kmPackage.name
    , description = kmPackage.description
    , organizationId = kmPackage.organizationId
    , kmId = kmPackage.kmId
    , version = kmPackage.version
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

module Wizard.Api.Models.KnowledgeModelPackageDetail exposing
    ( KnowledgeModelPackageDetail
    , createFormOptions
    , decoder
    , encode
    , getLatestPackageId
    , toPackage
    , toPackageSuggestion
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import List.Extra as List
import Time
import Version exposing (Version)
import Wizard.Api.Models.KnowledgeModelPackage exposing (KnowledgeModelPackage)
import Wizard.Api.Models.KnowledgeModelPackage.KnowledgeModelPackagePhase as KnoweldgeModelPackagePhase exposing (KnowledgeModelPackagePhase)
import Wizard.Api.Models.KnowledgeModelPackageSuggestion exposing (KnowledgeModelPackageSuggestion)
import Wizard.Api.Models.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)


type alias KnowledgeModelPackageDetail =
    { id : String
    , name : String
    , organizationId : String
    , kmId : String
    , version : Version
    , description : String
    , readme : String
    , license : String
    , metamodelVersion : Int
    , forkOfKnowledgeModelPackageId : Maybe String
    , previousKnowledgeModelPackageId : Maybe String
    , versions : List Version
    , organization : Maybe OrganizationInfo
    , registryLink : Maybe String
    , remoteLatestVersion : Maybe Version
    , phase : KnowledgeModelPackagePhase
    , nonEditable : Bool
    }


decoder : Decoder KnowledgeModelPackageDetail
decoder =
    D.succeed KnowledgeModelPackageDetail
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "organizationId" D.string
        |> D.required "kmId" D.string
        |> D.required "version" Version.decoder
        |> D.required "description" D.string
        |> D.required "readme" D.string
        |> D.required "license" D.string
        |> D.required "metamodelVersion" D.int
        |> D.required "forkOfKnowledgeModelPackageId" (D.maybe D.string)
        |> D.required "previousKnowledgeModelPackageId" (D.maybe D.string)
        |> D.required "versions" (D.list Version.decoder)
        |> D.required "organization" (D.maybe OrganizationInfo.decoder)
        |> D.required "registryLink" (D.maybe D.string)
        |> D.required "remoteLatestVersion" (D.maybe Version.decoder)
        |> D.required "phase" KnoweldgeModelPackagePhase.decoder
        |> D.required "nonEditable" D.bool


encode : { a | phase : KnowledgeModelPackagePhase } -> E.Value
encode kmPackage =
    E.object
        [ ( "phase", KnoweldgeModelPackagePhase.encode kmPackage.phase ) ]


createFormOptions : KnowledgeModelPackageDetail -> List ( String, String )
createFormOptions kmPackage =
    kmPackage.versions
        |> List.sortWith Version.compare
        |> List.filter (Version.greaterThan kmPackage.version)
        |> List.map (createFormOption kmPackage)


toPackage : KnowledgeModelPackageDetail -> KnowledgeModelPackage
toPackage kmPackage =
    { id = kmPackage.id
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
    }


toPackageSuggestion : KnowledgeModelPackageDetail -> KnowledgeModelPackageSuggestion
toPackageSuggestion kmPackage =
    { id = kmPackage.id
    , name = kmPackage.name
    , description = kmPackage.description
    , version = kmPackage.version
    }


createFormOption : KnowledgeModelPackageDetail -> Version -> ( String, String )
createFormOption kmPackage version =
    let
        id =
            kmPackage.organizationId ++ ":" ++ kmPackage.kmId ++ ":" ++ Version.toString version

        optionText =
            kmPackage.name ++ " " ++ Version.toString version ++ " (" ++ id ++ ")"
    in
    ( id, optionText )


getLatestVersion : KnowledgeModelPackageDetail -> Maybe Version
getLatestVersion =
    List.last << List.sortWith Version.compare << .versions


getLatestPackageId : KnowledgeModelPackageDetail -> Maybe String
getLatestPackageId kmPackage =
    case ( String.split ":" kmPackage.id, getLatestVersion kmPackage ) of
        ( orgId :: kmId :: _, Just latestVersion ) ->
            Just (orgId ++ ":" ++ kmId ++ ":" ++ Version.toString latestVersion)

        _ ->
            Nothing

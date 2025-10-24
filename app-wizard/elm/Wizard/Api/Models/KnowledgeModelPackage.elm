module Wizard.Api.Models.KnowledgeModelPackage exposing
    ( KnowledgeModelPackage
    , decoder
    , dummy
    , isOutdated
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Version exposing (Version)
import Wizard.Api.Models.KnowledgeModelPackage.KnowledgeModelPackagePhase as KnowledgeModelPackagePhase exposing (KnowledgeModelPackagePhase)
import Wizard.Api.Models.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)


type alias KnowledgeModelPackage =
    { id : String
    , name : String
    , organizationId : String
    , kmId : String
    , version : Version
    , description : String
    , organization : Maybe OrganizationInfo
    , remoteLatestVersion : Maybe Version
    , phase : KnowledgeModelPackagePhase
    , createdAt : Time.Posix
    , nonEditable : Bool
    }


decoder : Decoder KnowledgeModelPackage
decoder =
    D.succeed KnowledgeModelPackage
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "organizationId" D.string
        |> D.required "kmId" D.string
        |> D.required "version" Version.decoder
        |> D.required "description" D.string
        |> D.required "organization" (D.maybe OrganizationInfo.decoder)
        |> D.required "remoteLatestVersion" (D.maybe Version.decoder)
        |> D.required "phase" KnowledgeModelPackagePhase.decoder
        |> D.required "createdAt" D.datetime
        |> D.required "nonEditable" D.bool


dummy : KnowledgeModelPackage
dummy =
    { id = ""
    , name = ""
    , organizationId = ""
    , kmId = ""
    , version = Version.create 0 0 0
    , description = ""
    , organization = Nothing
    , remoteLatestVersion = Nothing
    , phase = KnowledgeModelPackagePhase.Released
    , createdAt = Time.millisToPosix 0
    , nonEditable = True
    }


isOutdated : { a | remoteLatestVersion : Maybe Version, version : Version } -> Bool
isOutdated kmPackage =
    case kmPackage.remoteLatestVersion of
        Just remoteLatestVersion ->
            Version.greaterThan kmPackage.version remoteLatestVersion

        Nothing ->
            False

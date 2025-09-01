module Wizard.Api.Models.Package exposing
    ( Package
    , decoder
    , dummy
    , isOutdated
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Version exposing (Version)
import Wizard.Api.Models.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)
import Wizard.Api.Models.Package.PackagePhase as PackagePhase exposing (PackagePhase)


type alias Package =
    { id : String
    , name : String
    , organizationId : String
    , kmId : String
    , version : Version
    , description : String
    , organization : Maybe OrganizationInfo
    , remoteLatestVersion : Maybe Version
    , phase : PackagePhase
    , createdAt : Time.Posix
    , nonEditable : Bool
    }


decoder : Decoder Package
decoder =
    D.succeed Package
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "organizationId" D.string
        |> D.required "kmId" D.string
        |> D.required "version" Version.decoder
        |> D.required "description" D.string
        |> D.required "organization" (D.maybe OrganizationInfo.decoder)
        |> D.required "remoteLatestVersion" (D.maybe Version.decoder)
        |> D.required "phase" PackagePhase.decoder
        |> D.required "createdAt" D.datetime
        |> D.required "nonEditable" D.bool


dummy : Package
dummy =
    { id = ""
    , name = ""
    , organizationId = ""
    , kmId = ""
    , version = Version.create 0 0 0
    , description = ""
    , organization = Nothing
    , remoteLatestVersion = Nothing
    , phase = PackagePhase.Released
    , createdAt = Time.millisToPosix 0
    , nonEditable = True
    }


isOutdated : { a | remoteLatestVersion : Maybe Version, version : Version } -> Bool
isOutdated template =
    case template.remoteLatestVersion of
        Just remoteLatestVersion ->
            Version.greaterThan template.version remoteLatestVersion

        Nothing ->
            False

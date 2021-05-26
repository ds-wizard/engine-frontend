module Shared.Data.PackageDetail exposing
    ( PackageDetail
    , createFormOptions
    , decoder
    , toPackage
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)
import Shared.Data.Package exposing (Package)
import Shared.Data.Package.PackageState as PackageState exposing (PackageState)
import Time
import Version exposing (Version)


type alias PackageDetail =
    { id : String
    , name : String
    , organizationId : String
    , kmId : String
    , version : Version
    , description : String
    , readme : String
    , license : String
    , metamodelVersion : Int
    , forkOfPackageId : Maybe String
    , previousPackageId : Maybe String
    , versions : List Version
    , organization : Maybe OrganizationInfo
    , registryLink : Maybe String
    , remoteLatestVersion : Maybe Version
    , state : PackageState
    }


decoder : Decoder PackageDetail
decoder =
    D.succeed PackageDetail
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "organizationId" D.string
        |> D.required "kmId" D.string
        |> D.required "version" Version.decoder
        |> D.required "description" D.string
        |> D.required "readme" D.string
        |> D.required "license" D.string
        |> D.required "metamodelVersion" D.int
        |> D.required "forkOfPackageId" (D.maybe D.string)
        |> D.required "previousPackageId" (D.maybe D.string)
        |> D.required "versions" (D.list Version.decoder)
        |> D.required "organization" (D.maybe OrganizationInfo.decoder)
        |> D.required "registryLink" (D.maybe D.string)
        |> D.required "remoteLatestVersion" (D.maybe Version.decoder)
        |> D.required "state" PackageState.decoder


createFormOptions : PackageDetail -> List ( String, String )
createFormOptions package =
    package.versions
        |> List.sortWith Version.compare
        |> List.filter (Version.greaterThan package.version)
        |> List.map (createFormOption package)


toPackage : PackageDetail -> Package
toPackage package =
    { id = package.id
    , name = package.name
    , organizationId = package.organizationId
    , kmId = package.kmId
    , version = package.version
    , description = package.description
    , versions = package.versions
    , organization = package.organization
    , remoteLatestVersion = Maybe.map Version.toString package.remoteLatestVersion
    , state = package.state
    , createdAt = Time.millisToPosix 0
    }


createFormOption : PackageDetail -> Version -> ( String, String )
createFormOption package version =
    let
        id =
            package.organizationId ++ ":" ++ package.kmId ++ ":" ++ Version.toString version

        optionText =
            package.name ++ " " ++ Version.toString version ++ " (" ++ id ++ ")"
    in
    ( id, optionText )

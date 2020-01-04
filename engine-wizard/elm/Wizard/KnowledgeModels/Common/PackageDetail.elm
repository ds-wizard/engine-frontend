module Wizard.KnowledgeModels.Common.PackageDetail exposing
    ( PackageDetail
    , createFormOptions
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Version exposing (Version)
import Wizard.KnowledgeModels.Common.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)
import Wizard.KnowledgeModels.Common.PackageState as PackageState exposing (PackageState)


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


createFormOption : PackageDetail -> Version -> ( String, String )
createFormOption package version =
    let
        id =
            package.organizationId ++ ":" ++ package.kmId ++ ":" ++ Version.toString version

        optionText =
            package.name ++ " " ++ Version.toString version ++ " (" ++ id ++ ")"
    in
    ( id, optionText )

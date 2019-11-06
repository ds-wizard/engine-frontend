module Wizard.KnowledgeModels.Common.Package exposing
    ( Package
    , createFormOption
    , decoder
    , dummy
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Version exposing (Version)
import Wizard.KnowledgeModels.Common.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)
import Wizard.KnowledgeModels.Common.PackageState as PackageState exposing (PackageState)


type alias Package =
    { id : String
    , name : String
    , organizationId : String
    , kmId : String
    , version : Version
    , description : String
    , versions : List Version
    , organization : Maybe OrganizationInfo
    , state : PackageState
    , createdAt : Time.Posix
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
        |> D.required "versions" (D.list Version.decoder)
        |> D.required "organization" (D.maybe OrganizationInfo.decoder)
        |> D.required "state" PackageState.decoder
        |> D.required "createdAt" D.datetime


dummy : Package
dummy =
    { id = ""
    , name = ""
    , organizationId = ""
    , kmId = ""
    , version = Version.create 0 0 0
    , description = ""
    , versions = []
    , organization = Nothing
    , state = PackageState.unknown
    , createdAt = Time.millisToPosix 0
    }


createFormOption : Package -> ( String, String )
createFormOption package =
    let
        optionText =
            package.name ++ " " ++ Version.toString package.version ++ " (" ++ package.id ++ ")"
    in
    ( package.id, optionText )

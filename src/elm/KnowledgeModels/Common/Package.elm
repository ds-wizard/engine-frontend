module KnowledgeModels.Common.Package exposing
    ( Package
    , decoder
    , dummy
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import KnowledgeModels.Common.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)
import KnowledgeModels.Common.PackageState as PackageState exposing (PackageState)
import KnowledgeModels.Common.Version as Version exposing (Version)
import Time


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

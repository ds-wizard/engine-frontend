module KnowledgeModels.Common.Package exposing
    ( Package
    , decoder
    , dummy
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import KnowledgeModels.Common.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)
import KnowledgeModels.Common.Version as Version exposing (Version)


type alias Package =
    { id : String
    , name : String
    , organizationId : String
    , kmId : String
    , version : Version
    , description : String
    , organization : Maybe OrganizationInfo
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


dummy : Package
dummy =
    { id = ""
    , name = ""
    , organizationId = ""
    , kmId = ""
    , version = Version.create 0 0 0
    , description = ""
    , organization = Nothing
    }

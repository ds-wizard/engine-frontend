module Registry.Api.Models.DocumentTemplate exposing
    ( DocumentTemplate
    , decoder
    , getId
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Registry.Api.Models.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)
import Time
import Uuid exposing (Uuid)
import Version exposing (Version)


type alias DocumentTemplate =
    { uuid : Uuid
    , name : String
    , templateId : String
    , version : Version
    , description : String
    , organization : OrganizationInfo
    , createdAt : Time.Posix
    }


decoder : Decoder DocumentTemplate
decoder =
    D.succeed DocumentTemplate
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "templateId" D.string
        |> D.required "version" Version.decoder
        |> D.required "description" D.string
        |> D.required "organization" OrganizationInfo.decoder
        |> D.required "createdAt" D.datetime


getId : DocumentTemplate -> String
getId documentTemplate =
    documentTemplate.organization.organizationId ++ ":" ++ documentTemplate.templateId ++ ":" ++ Version.toString documentTemplate.version

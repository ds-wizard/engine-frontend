module Registry.Api.Models.DocumentTemplateDetail exposing
    ( DocumentTemplateDetail
    , decoder
    , getId
    , otherVersionId
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Registry.Api.Models.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)
import Time
import Uuid exposing (Uuid)
import Version exposing (Version)


type alias DocumentTemplateDetail =
    { uuid : Uuid
    , name : String
    , templateId : String
    , version : Version
    , description : String
    , organization : OrganizationInfo
    , metamodelVersion : Version
    , readme : String
    , versions : List Version
    , license : String
    , createdAt : Time.Posix
    }


decoder : Decoder DocumentTemplateDetail
decoder =
    D.succeed DocumentTemplateDetail
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "templateId" D.string
        |> D.required "version" Version.decoder
        |> D.required "description" D.string
        |> D.required "organization" OrganizationInfo.decoder
        |> D.required "metamodelVersion" Version.decoder
        |> D.required "readme" D.string
        |> D.required "versions" (D.list Version.decoder)
        |> D.required "license" D.string
        |> D.required "createdAt" D.datetime


getId : DocumentTemplateDetail -> String
getId documentTemplate =
    documentTemplate.organization.organizationId ++ ":" ++ documentTemplate.templateId ++ ":" ++ Version.toString documentTemplate.version


otherVersionId : DocumentTemplateDetail -> Version -> String
otherVersionId documentTemplate version =
    documentTemplate.organization.organizationId ++ ":" ++ documentTemplate.templateId ++ ":" ++ Version.toString version

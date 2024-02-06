module Registry2.Api.Models.DocumentTemplateDetail exposing
    ( DocumentTemplateDetail
    , decoder
    , otherVersionId
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Registry2.Api.Models.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)
import Time
import Version exposing (Version)


type alias DocumentTemplateDetail =
    { id : String
    , name : String
    , templateId : String
    , version : Version
    , description : String
    , organization : OrganizationInfo
    , metamodelVersion : Int
    , readme : String
    , versions : List Version
    , license : String
    , createdAt : Time.Posix
    }


decoder : Decoder DocumentTemplateDetail
decoder =
    D.succeed DocumentTemplateDetail
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "templateId" D.string
        |> D.required "version" Version.decoder
        |> D.required "description" D.string
        |> D.required "organization" OrganizationInfo.decoder
        |> D.required "metamodelVersion" D.int
        |> D.required "readme" D.string
        |> D.required "versions" (D.list Version.decoder)
        |> D.required "license" D.string
        |> D.required "createdAt" D.datetime


otherVersionId : DocumentTemplateDetail -> Version -> String
otherVersionId documentTemplate version =
    documentTemplate.organization.organizationId ++ ":" ++ documentTemplate.templateId ++ ":" ++ Version.toString version

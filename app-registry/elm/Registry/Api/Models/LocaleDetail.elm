module Registry.Api.Models.LocaleDetail exposing
    ( LocaleDetail
    , decoder
    , otherVersionId
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Registry.Api.Models.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)
import Time
import Version exposing (Version)


type alias LocaleDetail =
    { id : String
    , name : String
    , code : String
    , localeId : String
    , version : Version
    , description : String
    , organization : OrganizationInfo
    , readme : String
    , versions : List Version
    , license : String
    , createdAt : Time.Posix
    }


decoder : Decoder LocaleDetail
decoder =
    D.succeed LocaleDetail
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "code" D.string
        |> D.required "localeId" D.string
        |> D.required "version" Version.decoder
        |> D.required "description" D.string
        |> D.required "organization" OrganizationInfo.decoder
        |> D.required "readme" D.string
        |> D.required "versions" (D.list Version.decoder)
        |> D.required "license" D.string
        |> D.required "createdAt" D.datetime


otherVersionId : LocaleDetail -> Version -> String
otherVersionId documentTemplate version =
    documentTemplate.organization.organizationId ++ ":" ++ documentTemplate.localeId ++ ":" ++ Version.toString version

module Registry.Api.Models.LocaleDetail exposing
    ( LocaleDetail
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


type alias LocaleDetail =
    { uuid : Uuid
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
        |> D.required "uuid" Uuid.decoder
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


getId : LocaleDetail -> String
getId locale =
    locale.organization.organizationId ++ ":" ++ locale.localeId ++ ":" ++ Version.toString locale.version


otherVersionId : LocaleDetail -> Version -> String
otherVersionId locale version =
    locale.organization.organizationId ++ ":" ++ locale.localeId ++ ":" ++ Version.toString version

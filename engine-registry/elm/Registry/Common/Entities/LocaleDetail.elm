module Registry.Common.Entities.LocaleDetail exposing
    ( LocaleDetail
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Shared.Data.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)
import Time
import Version exposing (Version)


type alias LocaleDetail =
    { id : String
    , localeId : String
    , name : String
    , description : String
    , code : String
    , organizationId : String
    , version : Version
    , createdAt : Time.Posix
    , organization : OrganizationInfo
    , license : String
    , readme : String
    , recommendedAppVersion : Version
    , versions : List Version
    }


decoder : Decoder LocaleDetail
decoder =
    D.succeed LocaleDetail
        |> D.required "id" D.string
        |> D.required "localeId" D.string
        |> D.required "name" D.string
        |> D.required "description" D.string
        |> D.required "code" D.string
        |> D.required "organizationId" D.string
        |> D.required "version" Version.decoder
        |> D.required "createdAt" D.datetime
        |> D.required "organization" OrganizationInfo.decoder
        |> D.required "license" D.string
        |> D.required "readme" D.string
        |> D.required "recommendedAppVersion" Version.decoder
        |> D.required "versions" (D.list Version.decoder)

module Registry.Api.Models.Locale exposing
    ( Locale
    , decoder
    , toItem
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Registry.Api.Models.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)
import Time
import Uuid exposing (Uuid)
import Version exposing (Version)


type alias Locale =
    { uuid : Uuid
    , name : String
    , code : String
    , localeId : String
    , version : Version
    , description : String
    , organization : OrganizationInfo
    , createdAt : Time.Posix
    }


decoder : Decoder Locale
decoder =
    D.succeed Locale
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "code" D.string
        |> D.required "localeId" D.string
        |> D.required "version" Version.decoder
        |> D.required "description" D.string
        |> D.required "organization" OrganizationInfo.decoder
        |> D.required "createdAt" D.datetime


toItem :
    Locale
    ->
        { createdAt : Time.Posix
        , description : String
        , id : String
        , name : String
        , organization : OrganizationInfo
        , version : Version
        }
toItem locale =
    { id = locale.organization.organizationId ++ ":" ++ locale.localeId ++ ":" ++ Version.toString locale.version
    , name = locale.name
    , description = locale.description
    , organization = locale.organization
    , version = locale.version
    , createdAt = locale.createdAt
    }
